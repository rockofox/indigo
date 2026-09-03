{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LLVM.Compiler
    ( compileInstructionsToModule
    , compileInstructionsToLLVM
    )
where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.State
import Data.Char (ord)
import Data.Int (Int64)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import LLVM.Emitter (emitModule)
import LLVM.Sanitizer (escapeLLVMString, sanitizeLabel)
import LLVM.Types
import VM
    ( Action (..)
    , Data (..)
    , Instruction (..)
    )

data CodegenState = CodegenState
    { stringMap :: Map.Map String (String, Int)
    , nextStrId :: Int
    , nextLblId :: Int
    , nextTempId :: Int
    , varSlotMap :: Map.Map String Int
    , nextSlotId :: Int
    , definedLabels :: [String]
    , unresolvedLabels :: Set.Set String
    , currentBlockName :: String
    , currentBlockInstrs :: [LLVMInstr]
    , completedBlocks :: [BasicBlock]
    , blockTerminated :: Bool
    }

type Codegen a = State CodegenState a

initCodegenState :: [String] -> CodegenState
initCodegenState defs =
    CodegenState
        { stringMap = Map.empty
        , nextStrId = 0
        , nextLblId = 0
        , nextTempId = 0
        , varSlotMap = Map.empty
        , nextSlotId = 0
        , definedLabels = defs
        , unresolvedLabels = Set.empty
        , currentBlockName = "entry"
        , currentBlockInstrs = []
        , completedBlocks = []
        , blockTerminated = False
        }

freshTemp :: Codegen String
freshTemp = do
    st <- get
    let t = "t" ++ show (nextTempId st)
    put $ st{nextTempId = nextTempId st + 1}
    return t

freshLocalLabel :: String -> Codegen String
freshLocalLabel prefix = do
    st <- get
    let lbl = prefix ++ "_" ++ show (nextLblId st)
    put $ st{nextLblId = nextLblId st + 1}
    return lbl

internString :: String -> Codegen (String, Int)
internString str = do
    st <- get
    case Map.lookup str (stringMap st) of
        Just info -> return info
        Nothing -> do
            let lbl = ".Lstr_" ++ show (nextStrId st)
                (_, len) = escapeLLVMString str
                info = (lbl, len)
            put $
                st
                    { stringMap = Map.insert str (lbl, len) (stringMap st)
                    , nextStrId = nextStrId st + 1
                    }
            return info

getVarSlot :: String -> Codegen Int
getVarSlot name = do
    st <- get
    case Map.lookup name (varSlotMap st) of
        Just slot -> return slot
        Nothing -> do
            let slot = nextSlotId st
            put $ st{varSlotMap = Map.insert name slot (varSlotMap st), nextSlotId = slot + 1}
            return slot

resolveLabel :: [String] -> String -> String
resolveLabel defined target
    | target `elem` defined = target
    | hasHash = case idMatches of
        (m : _) -> m
        [] -> target
    | not (null prefixMatches) = case prefixMatches of
        (m : _) -> m
        [] -> target
    | otherwise = target
  where
    hasHash = '#' `elem` target
    targetPrefix = takeWhile (/= '#') target
    prefixMatches = filter (\l -> takeWhile (/= '#') l == targetPrefix) defined
    targetId = case dropWhile (/= '#') target of
        ('#' : rest) -> rest
        _ -> ""
    idMatches =
        if null targetId
            then []
            else filter (\l -> case dropWhile (/= '#') l of ('#' : r) -> r == targetId; _ -> False) defined

resolveTarget :: String -> Codegen String
resolveTarget target = do
    defs <- gets definedLabels
    let resolved = resolveLabel defs target
    unless (resolved `elem` defs) $ do
        modify $ \s -> s{unresolvedLabels = Set.insert resolved (unresolvedLabels s)}
    return resolved

emitInstr :: LLVMInstr -> Codegen ()
emitInstr instr = do
    term <- gets blockTerminated
    unless term $ do
        modify $ \s -> s{currentBlockInstrs = currentBlockInstrs s ++ [instr]}
        when (isTerminator instr) $
            modify $
                \s -> s{blockTerminated = True}
  where
    isTerminator = \case
        BrOp _ -> True
        BrCondOp{} -> True
        RetOp _ -> True
        UnreachableOp -> True
        _ -> False

startBlock :: String -> Codegen ()
startBlock name = do
    st <- get
    let currentInstrs = currentBlockInstrs st
        curName = currentBlockName st
        term = blockTerminated st
        sanitized = sanitizeLabel name
    unless (null curName && null currentInstrs) $ do
        unless term $ do
            modify $ \s -> s{currentBlockInstrs = currentBlockInstrs s ++ [BrOp sanitized]}
        st' <- get
        let bb = BasicBlock curName (currentBlockInstrs st')
        modify $ \s -> s{completedBlocks = completedBlocks s ++ [bb]}
    modify $ \s ->
        s
            { currentBlockName = sanitized
            , currentBlockInstrs = []
            , blockTerminated = False
            }

finishBlocks :: Codegen [BasicBlock]
finishBlocks = do
    st <- get
    let term = blockTerminated st
        curName = currentBlockName st
    unless term $ do
        modify $ \s -> s{currentBlockInstrs = currentBlockInstrs s ++ [RetOp Nothing]}
    st' <- get
    let bb = BasicBlock curName (currentBlockInstrs st')
        allBlocks = completedBlocks st' ++ [bb]
    modify $ \s ->
        s
            { currentBlockName = "entry"
            , currentBlockInstrs = []
            , completedBlocks = []
            , blockTerminated = False
            }
    return allBlocks

compileInstruction :: Set.Set String -> Instruction -> Codegen ()
compileInstruction _ (Push d) = compilePushData d
compileInstruction _ Pop = do
    tSp <- freshTemp
    emitInstr $ LoadOp I64 tSp (GlobalVar "g_sp")
    tSpNew <- freshTemp
    emitInstr $ SubOp I64 tSpNew (LocalVar tSp) (ConstInt 1)
    emitInstr $ StoreOp I64 (LocalVar tSpNew) (GlobalVar "g_sp")
compileInstruction _ Dup =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_dup") []
compileInstruction _ Swp =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_swp") []
compileInstruction _ Add =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_add") []
compileInstruction _ Sub =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_sub") []
compileInstruction _ Mul =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_mul") []
compileInstruction _ Div =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_div") []
compileInstruction _ Mod =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_mod") []
compileInstruction _ Pow =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_pow") []
compileInstruction _ Abs =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_abs") []
compileInstruction _ Eq =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_eq") []
compileInstruction _ Neq =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_neq") []
compileInstruction _ Lt =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_lt") []
compileInstruction _ Gt =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_gt") []
compileInstruction _ Not =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_not") []
compileInstruction _ And =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_and") []
compileInstruction _ Or =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_or") []
compileInstruction _ (Label name) =
    startBlock name
compileInstruction localLabels (Jmp name) = do
    tgt <- resolveTarget name
    let localCandidates = Set.toList localLabels
        resolvedLocal = resolveLabel localCandidates name
    if resolvedLocal `Set.member` localLabels
        then emitInstr $ BrOp (sanitizeLabel resolvedLocal)
        else
            if tgt `Set.member` localLabels
                then emitInstr $ BrOp (sanitizeLabel tgt)
                else do
                    emitInstr $ CallOp Void Nothing (GlobalVar (sanitizeLabel tgt)) []
                    emitInstr $ RetOp Nothing
compileInstruction localLabels (Jt name) = do
    tgt <- resolveTarget name
    skipLbl <- freshLocalLabel "jt_skip"
    tSp <- freshTemp
    emitInstr $ LoadOp I64 tSp (GlobalVar "g_sp")
    tSpNew <- freshTemp
    emitInstr $ SubOp I64 tSpNew (LocalVar tSp) (ConstInt 1)
    emitInstr $ StoreOp I64 (LocalVar tSpNew) (GlobalVar "g_sp")
    tValPtr <- freshTemp
    emitInstr $ GetElementPtrOp (NamedType "%struct.IndigoValue") tValPtr (GlobalVar "g_stack") [LocalVar tSpNew]
    tPayloadPtr <- freshTemp
    emitInstr $ GetElementPtrOp I64 tPayloadPtr (LocalVar tValPtr) [ConstInt 1]
    tPayload <- freshTemp
    emitInstr $ LoadOp I64 tPayload (LocalVar tPayloadPtr)
    tCond <- freshTemp
    emitInstr $ ICmpOp "ne" I64 tCond (LocalVar tPayload) (ConstInt 0)
    let localCandidates = Set.toList localLabels
        resolvedLocal = resolveLabel localCandidates name
        isLocal = resolvedLocal `Set.member` localLabels || tgt `Set.member` localLabels
        targetLabel = if resolvedLocal `Set.member` localLabels then resolvedLocal else tgt
    if isLocal
        then do
            emitInstr $ BrCondOp (LocalVar tCond) (sanitizeLabel targetLabel) (sanitizeLabel skipLbl)
            startBlock skipLbl
        else do
            callLbl <- freshLocalLabel "jt_call"
            emitInstr $ BrCondOp (LocalVar tCond) (sanitizeLabel callLbl) (sanitizeLabel skipLbl)
            startBlock callLbl
            emitInstr $ CallOp Void Nothing (GlobalVar (sanitizeLabel tgt)) []
            emitInstr $ RetOp Nothing
            startBlock skipLbl
compileInstruction localLabels (Jf name) = do
    tgt <- resolveTarget name
    skipLbl <- freshLocalLabel "jf_skip"
    tSp <- freshTemp
    emitInstr $ LoadOp I64 tSp (GlobalVar "g_sp")
    tSpNew <- freshTemp
    emitInstr $ SubOp I64 tSpNew (LocalVar tSp) (ConstInt 1)
    emitInstr $ StoreOp I64 (LocalVar tSpNew) (GlobalVar "g_sp")
    tValPtr <- freshTemp
    emitInstr $ GetElementPtrOp (NamedType "%struct.IndigoValue") tValPtr (GlobalVar "g_stack") [LocalVar tSpNew]
    tPayloadPtr <- freshTemp
    emitInstr $ GetElementPtrOp I64 tPayloadPtr (LocalVar tValPtr) [ConstInt 1]
    tPayload <- freshTemp
    emitInstr $ LoadOp I64 tPayload (LocalVar tPayloadPtr)
    tCond <- freshTemp
    emitInstr $ ICmpOp "eq" I64 tCond (LocalVar tPayload) (ConstInt 0)
    let localCandidates = Set.toList localLabels
        resolvedLocal = resolveLabel localCandidates name
        isLocal = resolvedLocal `Set.member` localLabels || tgt `Set.member` localLabels
        targetLabel = if resolvedLocal `Set.member` localLabels then resolvedLocal else tgt
    if isLocal
        then do
            emitInstr $ BrCondOp (LocalVar tCond) (sanitizeLabel targetLabel) (sanitizeLabel skipLbl)
            startBlock skipLbl
        else do
            callLbl <- freshLocalLabel "jf_call"
            emitInstr $ BrCondOp (LocalVar tCond) (sanitizeLabel callLbl) (sanitizeLabel skipLbl)
            startBlock callLbl
            emitInstr $ CallOp Void Nothing (GlobalVar (sanitizeLabel tgt)) []
            emitInstr $ RetOp Nothing
            startBlock skipLbl
compileInstruction _ (Call name) = do
    tgt <- resolveTarget name
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_enter_frame") []
    emitInstr $ CallOp Void Nothing (GlobalVar (sanitizeLabel tgt)) []
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_leave_frame") []
compileInstruction _ (CallLocal name) = do
    tgt <- resolveTarget name
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_enter_frame_inherit") []
    emitInstr $ CallOp Void Nothing (GlobalVar (sanitizeLabel tgt)) []
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_leave_frame") []
compileInstruction _ (TailCall name) = do
    tgt <- resolveTarget name
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_leave_frame") []
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_enter_frame") []
    emitInstr $ CallOp Void Nothing (GlobalVar (sanitizeLabel tgt)) []
    emitInstr $ RetOp Nothing
compileInstruction _ (TailCallLocal name) = do
    tgt <- resolveTarget name
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_leave_frame") []
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_enter_frame_inherit") []
    emitInstr $ CallOp Void Nothing (GlobalVar (sanitizeLabel tgt)) []
    emitInstr $ RetOp Nothing
compileInstruction _ CallS = do
    tPtr <- freshTemp
    emitInstr $ CallOp Ptr (Just tPtr) (GlobalVar "indigo_calls_prepare") []
    emitInstr $ CallOp Void Nothing (LocalVar tPtr) []
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_leave_frame") []
compileInstruction _ (PushPf name nArgs) = do
    tgt <- resolveTarget name
    (nameLbl, _) <- internString name
    emitInstr $
        CallOp
            Void
            Nothing
            (GlobalVar "indigo_push_pf")
            [ (Ptr, GlobalVar (sanitizeLabel tgt))
            , (Ptr, GlobalVar nameLbl)
            , (I64, ConstInt (fromIntegral nArgs))
            ]
compileInstruction _ Ret =
    emitInstr $ RetOp Nothing
compileInstruction _ Exit = do
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_exit") []
    emitInstr UnreachableOp
compileInstruction _ (LStore name) = do
    slot <- getVarSlot name
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_lstore_slot") [(I64, ConstInt (fromIntegral slot))]
compileInstruction _ (LLoad name) = do
    slot <- getVarSlot name
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_lload_slot") [(I64, ConstInt (fromIntegral slot))]
compileInstruction _ (Store idx) =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_store") [(I64, ConstInt (fromIntegral idx))]
compileInstruction _ (Load idx) =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_load") [(I64, ConstInt (fromIntegral idx))]
compileInstruction _ (VM.Mov idx dat) = do
    compilePushData dat
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_store") [(I64, ConstInt (fromIntegral idx))]
compileInstruction _ StoreSideStack =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_storesidestack") []
compileInstruction _ LoadSideStack =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_loadsidestack") []
compileInstruction _ ClearSideStack =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_clearsidestack") []
compileInstruction _ (Concat n) =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_concat") [(I64, ConstInt (fromIntegral n))]
compileInstruction _ (ListAdd n) =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_list_add") [(I64, ConstInt (fromIntegral n))]
compileInstruction _ (PackList n) =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_pack_list") [(I64, ConstInt (fromIntegral n))]
compileInstruction _ UnpackList =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_unpack_list") []
compileInstruction _ Index =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_index") []
compileInstruction _ Slice =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_slice") []
compileInstruction _ Length =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_length") []
compileInstruction _ (PackMap n) =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_pack_map") [(I64, ConstInt (fromIntegral n))]
compileInstruction _ (Access field) = do
    (fieldLbl, _) <- internString field
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_access") [(Ptr, GlobalVar fieldLbl)]
compileInstruction _ AAccess =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_aaccess") []
compileInstruction _ Keys =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_keys") []
compileInstruction _ Update =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_update") []
compileInstruction _ Cast =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_cast") []
compileInstruction _ TypeOf =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_typeof") []
compileInstruction _ TypeEq =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_type_eq") []
compileInstruction _ (Builtin Print) =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_builtin_print") []
compileInstruction _ (Builtin GetLine) =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_builtin_getline") []
compileInstruction _ (Builtin GetChar) =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_builtin_getchar") []
compileInstruction _ (Builtin Random) =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_builtin_random") []
#ifdef POSIX_IO
compileInstruction _ (Builtin _) =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_builtin_print") []
#endif
compileInstruction _ (Comment msg) =
    emitInstr $ CommentLine msg
compileInstruction _ (Meta msg) =
    emitInstr $ CommentLine ("meta: " ++ msg)
compileInstruction _ (CallFFI name _ numArgs) =
    emitInstr $ CallOp Void Nothing (GlobalVar ("indigo_ffi_" ++ name)) [(I64, ConstInt (fromIntegral numArgs))]

compilePushData :: Data -> Codegen ()
compilePushData (DInt n) =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_push") [(I64, ConstInt 1), (I64, ConstInt (fromIntegral n))]
compilePushData (DFloat f) =
    let w = fromIntegral (castFloatToWord32 f) :: Int64
     in emitInstr $ CallOp Void Nothing (GlobalVar "indigo_push") [(I64, ConstInt 2), (I64, ConstInt w)]
compilePushData (DDouble d) =
    let w = fromIntegral (castDoubleToWord64 d) :: Int64
     in emitInstr $ CallOp Void Nothing (GlobalVar "indigo_push") [(I64, ConstInt 3), (I64, ConstInt w)]
compilePushData (DString s) = do
    (lbl, _) <- internString s
    tVal <- freshTemp
    emitInstr $ PtrToIntOp tVal (GlobalVar lbl) I64
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_push") [(I64, ConstInt 4), (I64, LocalVar tVal)]
compilePushData (DBool b) =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_push") [(I64, ConstInt 5), (I64, ConstInt (if b then 1 else 0))]
compilePushData (DList xs) = do
    mapM_ compilePushData xs
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_pack_list") [(I64, ConstInt (fromIntegral (length xs)))]
compilePushData DNone =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_push") [(I64, ConstInt 0), (I64, ConstInt 0)]
compilePushData (DChar c) =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_push") [(I64, ConstInt 7), (I64, ConstInt (fromIntegral (ord c)))]
compilePushData (DCPtr p) =
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_push") [(I64, ConstInt 11), (I64, ConstInt (fromIntegral p))]
compilePushData (DTypeQuery q) = do
    (lbl, _) <- internString q
    tVal <- freshTemp
    emitInstr $ PtrToIntOp tVal (GlobalVar lbl) I64
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_push") [(I64, ConstInt 10), (I64, LocalVar tVal)]
compilePushData (DMap m) = do
    let elems = Map.toList m
    forM_ elems $ \(k, v) -> do
        compilePushData (DString k)
        compilePushData v
    emitInstr $ CallOp Void Nothing (GlobalVar "indigo_pack_map") [(I64, ConstInt (fromIntegral (length elems * 2)))]
compilePushData (DFuncRef name args _) = do
    mapM_ compilePushData args
    tgt <- resolveTarget name
    (nameLbl, _) <- internString name
    emitInstr $
        CallOp
            Void
            Nothing
            (GlobalVar "indigo_push_pf")
            [ (Ptr, GlobalVar (sanitizeLabel tgt))
            , (Ptr, GlobalVar nameLbl)
            , (I64, ConstInt (fromIntegral (length args)))
            ]

isFunctionStart :: Instruction -> Bool
isFunctionStart (Label name) = name == "main" || '#' `elem` name
isFunctionStart _ = False

groupFunctions :: [Instruction] -> [(String, [Instruction])]
groupFunctions = go
  where
    go [] = []
    go (Label name : rest)
        | isFunctionStart (Label name) =
            let (body, next) = break isFunctionStart rest
             in (name, body) : go next
        | otherwise =
            go rest
    go (_ : rest) = go rest

compileInstructionsToModule :: [Instruction] -> LLVMModule
compileInstructionsToModule instrs =
    let allDefs = [name | Label name <- instrs]
        (topLevel, funcs) = break isLabel instrs
        isLabel (Label _) = True
        isLabel _ = False
        fnGroups = groupFunctions funcs
        mainTarget = sanitizeLabel (resolveLabel allDefs "main")
        genAction = do
            tb <- do
                let topLocals = Set.fromList [l | Label l <- topLevel]
                mapM_ (compileInstruction topLocals) topLevel
                emitInstr $ CallOp Void Nothing (GlobalVar mainTarget) []
                emitInstr $ RetOp Nothing
                finishBlocks
            compiledUserFns <- forM fnGroups $ \(fnName, fnBody) -> do
                let bodyLocals = Set.fromList [l | Label l <- fnBody]
                mapM_ (compileInstruction bodyLocals) fnBody
                Function Void (sanitizeLabel fnName) [] <$> finishBlocks
            unresolved <- gets (Set.toList . unresolvedLabels)
            tf <- forM unresolved makeTrap
            return (tb, compiledUserFns, tf)
        ((topBlocks, userFns, trapFns), st) = runState genAction (initCodegenState allDefs)
        uniqueRodata =
            [ GlobalString lbl escaped len
            | (str, (lbl, len)) <- Map.toList (stringMap st)
            , let (escaped, _) = escapeLLVMString str
            ]
        mainFn =
            Function
                I32
                "main"
                []
                [ BasicBlock
                    "entry"
                    [ CallOp Void Nothing (GlobalVar "indigo_enter_frame") []
                    , CallOp Void Nothing (GlobalVar "indigo_entry") []
                    , CallOp Void Nothing (GlobalVar "indigo_leave_frame") []
                    , RetOp (Just (I32, ConstInt 0))
                    ]
                ]
        entryFn = Function Void "indigo_entry" [] topBlocks
        allFns = [mainFn, entryFn] ++ userFns ++ trapFns
     in LLVMModule{moduleName = "indigo_module", globals = uniqueRodata, functions = allFns}
  where
    makeTrap :: String -> Codegen Function
    makeTrap lbl = do
        (strLbl, _) <- internString lbl
        return $
            Function
                Void
                (sanitizeLabel lbl)
                []
                [ BasicBlock
                    "entry"
                    [ CallOp Void Nothing (GlobalVar "indigo_label_not_found") [(Ptr, GlobalVar strLbl)]
                    , UnreachableOp
                    ]
                ]

compileInstructionsToLLVM :: [Instruction] -> String
compileInstructionsToLLVM = emitModule . compileInstructionsToModule
