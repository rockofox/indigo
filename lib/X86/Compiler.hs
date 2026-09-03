{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module X86.Compiler (compileInstructionsToAsm, compileProgramToAssemblyFile) where

import Control.Monad (unless)
import Control.Monad.State
import Data.Char (ord)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Float (castDoubleToWord64, castFloatToWord32)
import VM
    ( Action (..)
    , Data (..)
    , Instruction
        ( AAccess
        , Abs
        , Access
        , Add
        , And
        , Builtin
        , Call
        , CallFFI
        , CallLocal
        , CallS
        , Cast
        , ClearSideStack
        , Comment
        , Concat
        , Div
        , Dup
        , Eq
        , Exit
        , Gt
        , Index
        , Jf
        , Jmp
        , Jt
        , Keys
        , LLoad
        , LStore
        , Label
        , Length
        , ListAdd
        , Load
        , LoadSideStack
        , Lt
        , Meta
        , Mod
        , Mul
        , Neq
        , Not
        , Or
        , PackList
        , PackMap
        , Pop
        , Pow
        , Push
        , PushPf
        , Ret
        , Slice
        , Store
        , StoreSideStack
        , Sub
        , Swp
        , TailCall
        , TailCallLocal
        , TypeEq
        , TypeOf
        , UnpackList
        , Update
        )
    )
import VM qualified
import X86.Sanitizer (sanitizeLabel)
import X86.Types

data CodegenState = CodegenState
    { stringMap :: Map.Map String String
    , nextStrId :: Int
    , nextLblId :: Int
    , varSlotMap :: Map.Map String Int
    , nextSlotId :: Int
    , definedLabels :: [String]
    , unresolvedLabels :: Set.Set String
    }

type Codegen a = State CodegenState a

initCodegenState :: [String] -> CodegenState
initCodegenState defs =
    CodegenState
        { stringMap = Map.empty
        , nextStrId = 0
        , nextLblId = 0
        , varSlotMap = Map.empty
        , nextSlotId = 0
        , definedLabels = defs
        , unresolvedLabels = Set.empty
        }

callC :: String -> [X86Instr]
callC name = [SubOp (Reg RSP) (Imm 8), CallOp name, AddOp (Reg RSP) (Imm 8)]

getVarSlot :: String -> Codegen Int
getVarSlot name = do
    st <- get
    case Map.lookup name (varSlotMap st) of
        Just slot -> return slot
        Nothing -> do
            let slot = nextSlotId st
            put $ st{varSlotMap = Map.insert name slot (varSlotMap st), nextSlotId = slot + 1}
            return slot

internString :: String -> Codegen String
internString str = do
    st <- get
    case Map.lookup str (stringMap st) of
        Just lbl -> return lbl
        Nothing -> do
            let lbl = ".Lstr_" ++ show (nextStrId st)
            put $
                st
                    { stringMap = Map.insert str lbl (stringMap st)
                    , nextStrId = nextStrId st + 1
                    }
            return lbl

freshLocalLabel :: String -> Codegen String
freshLocalLabel prefix = do
    st <- get
    let lbl = ".L" ++ prefix ++ "_" ++ show (nextLblId st)
    put $ st{nextLblId = nextLblId st + 1}
    return lbl

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

compileInstructionsToAsm :: [Instruction] -> AssemblyFile
compileInstructionsToAsm instrs =
    let allDefs = [name | Label name <- instrs]
        (topLevel, funcs) = break isLabel instrs
        isLabel (Label _) = True
        isLabel _ = False
        genAction = do
            topInstrs <- concatMapM compileInstruction topLevel
            funcInstrs <- concatMapM compileInstruction funcs
            unresolved <- gets (Set.toList . unresolvedLabels)
            traps <- concatMapM makeTrap unresolved
            return (topInstrs, funcInstrs, traps)
        ((topBody, funcBody, trapInstrs), st) = runState genAction (initCodegenState allDefs)
        rodata = [(lbl, str) | (str, lbl) <- Map.toList (stringMap st)]
        mainTarget = sanitizeLabel (resolveLabel allDefs "main")
        entryWrapper =
            [ LabelDef "main"
            , PushOp (Reg RBP)
            , Mov (Reg RBP) (Reg RSP)
            , CallOp "indigo_enter_frame"
            , CallOp "indigo_entry"
            , CallOp "indigo_leave_frame"
            , Mov (Reg RAX) (Imm 0)
            , PopOp (Reg RBP)
            , RetOp
            , LabelDef "indigo_entry"
            ]
                ++ topBody
                ++ [ JmpOp mainTarget
                   , RetOp
                   ]
        fullText = entryWrapper ++ funcBody ++ trapInstrs
     in AssemblyFile{textSection = fullText, rodataStrings = rodata}
  where
    makeTrap :: String -> Codegen [X86Instr]
    makeTrap lbl = do
        strLbl <- internString lbl
        return $
            [ LabelDef (sanitizeLabel lbl)
            , Lea (Reg RDI) (MemLabel strLbl)
            ]
                ++ callC "indigo_label_not_found"

compileProgramToAssemblyFile :: [Instruction] -> AssemblyFile
compileProgramToAssemblyFile = compileInstructionsToAsm

compileInstruction :: Instruction -> Codegen [X86Instr]
compileInstruction (Push d) = compilePushData d
compileInstruction Pop = return [RawAsm "sub qword ptr [rip + g_sp], 1"]
compileInstruction Dup =
    return
        [ RawAsm "mov rax, [rip + g_sp]"
        , RawAsm "shl rax, 3"
        , RawAsm "lea r11, [rip + g_stack]"
        , RawAsm "add r11, rax"
        , RawAsm "mov rbx, [r11 - 8]"
        , RawAsm "mov [r11], rbx"
        , RawAsm "add qword ptr [rip + g_sp], 1"
        ]
compileInstruction Swp =
    return
        [ RawAsm "mov rax, [rip + g_sp]"
        , RawAsm "shl rax, 3"
        , RawAsm "lea r11, [rip + g_stack]"
        , RawAsm "add r11, rax"
        , RawAsm "mov rbx, [r11 - 8]"
        , RawAsm "mov rdx, [r11 - 16]"
        , RawAsm "mov [r11 - 16], rbx"
        , RawAsm "mov [r11 - 8], rdx"
        ]
compileInstruction Add = return $ callC "indigo_add"
compileInstruction Sub = return $ callC "indigo_sub"
compileInstruction Mul = return $ callC "indigo_mul"
compileInstruction Div = return $ callC "indigo_div"
compileInstruction Mod = return $ callC "indigo_mod"
compileInstruction Pow = return $ callC "indigo_pow"
compileInstruction Abs = return $ callC "indigo_abs"
compileInstruction Eq = return $ callC "indigo_eq"
compileInstruction Neq = return $ callC "indigo_neq"
compileInstruction Lt = return $ callC "indigo_lt"
compileInstruction Gt = return $ callC "indigo_gt"
compileInstruction Not = return $ callC "indigo_not"
compileInstruction And = return $ callC "indigo_and"
compileInstruction Or = return $ callC "indigo_or"
compileInstruction (Label name) =
    return [LabelDef (sanitizeLabel name)]
compileInstruction (Jmp name) = do
    tgt <- resolveTarget name
    return [JmpOp (sanitizeLabel tgt)]
compileInstruction (Jt name) = do
    tgt <- resolveTarget name
    skipLbl <- freshLocalLabel "jt_skip"
    return $
        [ RawAsm "sub qword ptr [rip + g_sp], 1"
        , RawAsm "mov rax, [rip + g_sp]"
        , RawAsm "shl rax, 3"
        , RawAsm "lea r11, [rip + g_stack]"
        , RawAsm "mov rdi, [r11 + rax]"
        ]
            ++ callC "indigo_is_truthy"
            ++ [ RawAsm "cmp al, 0"
               , RawAsm ("je " ++ skipLbl)
               , JmpOp (sanitizeLabel tgt)
               , LabelDef skipLbl
               ]
compileInstruction (Jf name) = do
    tgt <- resolveTarget name
    skipLbl <- freshLocalLabel "jf_skip"
    return $
        [ RawAsm "sub qword ptr [rip + g_sp], 1"
        , RawAsm "mov rax, [rip + g_sp]"
        , RawAsm "shl rax, 3"
        , RawAsm "lea r11, [rip + g_stack]"
        , RawAsm "mov rdi, [r11 + rax]"
        ]
            ++ callC "indigo_is_truthy"
            ++ [ RawAsm "cmp al, 0"
               , RawAsm ("jne " ++ skipLbl)
               , JmpOp (sanitizeLabel tgt)
               , LabelDef skipLbl
               ]
compileInstruction (Call name) = do
    tgt <- resolveTarget name
    return $
        callC "indigo_enter_frame"
            ++ [SubOp (Reg RSP) (Imm 8), CallOp (sanitizeLabel tgt), AddOp (Reg RSP) (Imm 8)]
            ++ callC "indigo_leave_frame"
compileInstruction (CallLocal name) = do
    tgt <- resolveTarget name
    return $
        callC "indigo_enter_frame_inherit"
            ++ [SubOp (Reg RSP) (Imm 8), CallOp (sanitizeLabel tgt), AddOp (Reg RSP) (Imm 8)]
            ++ callC "indigo_leave_frame"
compileInstruction (TailCall name) = do
    tgt <- resolveTarget name
    return $
        callC "indigo_leave_frame"
            ++ callC "indigo_enter_frame"
            ++ [JmpOp (sanitizeLabel tgt)]
compileInstruction (TailCallLocal name) = do
    tgt <- resolveTarget name
    return $
        callC "indigo_leave_frame"
            ++ callC "indigo_enter_frame_inherit"
            ++ [JmpOp (sanitizeLabel tgt)]
compileInstruction CallS =
    return $
        callC "indigo_calls_prepare"
            ++ [SubOp (Reg RSP) (Imm 8), RawAsm "call rax", AddOp (Reg RSP) (Imm 8)]
            ++ callC "indigo_leave_frame"
compileInstruction (PushPf name nArgs) = do
    tgt <- resolveTarget name
    nameLbl <- internString name
    return $
        [ Lea (Reg RDI) (MemLabel (sanitizeLabel tgt))
        , Lea (Reg RSI) (MemLabel nameLbl)
        , Mov (Reg RDX) (Imm nArgs)
        ]
            ++ callC "indigo_push_pf"
compileInstruction Ret =
    return [RetOp]
compileInstruction Exit =
    return $ callC "indigo_exit"
compileInstruction (LStore name) = do
    slot <- getVarSlot name
    return $ Mov (Reg RDI) (Imm slot) : callC "indigo_lstore_slot"
compileInstruction (LLoad name) = do
    slot <- getVarSlot name
    return $ Mov (Reg RDI) (Imm slot) : callC "indigo_lload_slot"
compileInstruction (Store idx) =
    return $ Mov (Reg RDI) (Imm idx) : callC "indigo_store"
compileInstruction (Load idx) =
    return $ Mov (Reg RDI) (Imm idx) : callC "indigo_load"
compileInstruction (VM.Mov idx dat) = do
    pushInstrs <- compilePushData dat
    return $
        pushInstrs
            ++ [ Mov (Reg RDI) (Imm idx)
               ]
            ++ callC "indigo_store"
compileInstruction StoreSideStack =
    return $ callC "indigo_storesidestack"
compileInstruction LoadSideStack =
    return $ callC "indigo_loadsidestack"
compileInstruction ClearSideStack =
    return $ callC "indigo_clearsidestack"
compileInstruction (Concat n) =
    return $ Mov (Reg RDI) (Imm n) : callC "indigo_concat"
compileInstruction (ListAdd n) =
    return $ Mov (Reg RDI) (Imm n) : callC "indigo_list_add"
compileInstruction (PackList n) =
    return $ Mov (Reg RDI) (Imm n) : callC "indigo_pack_list"
compileInstruction UnpackList =
    return $ callC "indigo_unpack_list"
compileInstruction Index =
    return $ callC "indigo_index"
compileInstruction Slice =
    return $ callC "indigo_slice"
compileInstruction Length =
    return $ callC "indigo_length"
compileInstruction (PackMap n) =
    return $ Mov (Reg RDI) (Imm n) : callC "indigo_pack_map"
compileInstruction (Access field) = do
    fieldLbl <- internString field
    return $ Lea (Reg RDI) (MemLabel fieldLbl) : callC "indigo_access"
compileInstruction AAccess =
    return $ callC "indigo_aaccess"
compileInstruction Keys =
    return $ callC "indigo_keys"
compileInstruction Update =
    return $ callC "indigo_update"
compileInstruction Cast =
    return $ callC "indigo_cast"
compileInstruction TypeOf =
    return $ callC "indigo_typeof"
compileInstruction TypeEq =
    return $ callC "indigo_type_eq"
compileInstruction (Builtin Print) =
    return $ callC "indigo_builtin_print"
compileInstruction (Builtin GetLine) =
    return $ callC "indigo_builtin_getline"
compileInstruction (Builtin GetChar) =
    return $ callC "indigo_builtin_getchar"
compileInstruction (Builtin Random) =
    return $ callC "indigo_builtin_random"
#ifdef POSIX_IO
compileInstruction (Builtin _) =
    return $ callC "indigo_builtin_print"
#endif
compileInstruction (Comment msg) =
    return [CommentLine msg]
compileInstruction (Meta msg) =
    return [CommentLine ("meta: " ++ msg)]
compileInstruction (CallFFI name _ numArgs) =
    return $ Mov (Reg RDI) (Imm numArgs) : callC ("indigo_ffi_" ++ name)

compilePushData :: Data -> Codegen [X86Instr]
compilePushData (DInt n) =
    return $
        [ Mov (Reg RDI) (Imm 1)
        , Mov (Reg RSI) (Imm (fromIntegral n))
        ]
            ++ callC "indigo_push"
compilePushData (DFloat f) =
    let w = fromIntegral (castFloatToWord32 f) :: Int
     in return $
            [ Mov (Reg RDI) (Imm 2)
            , Mov (Reg RSI) (Imm (fromIntegral w))
            ]
                ++ callC "indigo_push"
compilePushData (DDouble d) =
    let w = fromIntegral (castDoubleToWord64 d) :: Int
     in return $
            [ Mov (Reg RDI) (Imm 3)
            , Mov (Reg RSI) (Imm (fromIntegral w))
            ]
                ++ callC "indigo_push"
compilePushData (DString s) = do
    lbl <- internString s
    return $
        [ Mov (Reg RDI) (Imm 4)
        , Lea (Reg RSI) (MemLabel lbl)
        ]
            ++ callC "indigo_push"
compilePushData (DBool b) =
    return $
        [ Mov (Reg RDI) (Imm 5)
        , Mov (Reg RSI) (Imm (if b then 1 else 0))
        ]
            ++ callC "indigo_push"
compilePushData (DList xs) = do
    pushElems <- concatMapM compilePushData xs
    return $
        pushElems
            ++ [ Mov (Reg RDI) (Imm (length xs))
               ]
            ++ callC "indigo_pack_list"
compilePushData DNone =
    return $
        [ Mov (Reg RDI) (Imm 0)
        , Mov (Reg RSI) (Imm 0)
        ]
            ++ callC "indigo_push"
compilePushData (DChar c) =
    return $
        [ Mov (Reg RDI) (Imm 7)
        , Mov (Reg RSI) (Imm (ord c))
        ]
            ++ callC "indigo_push"
compilePushData (DCPtr p) =
    return $
        [ Mov (Reg RDI) (Imm 11)
        , Mov (Reg RSI) (Imm (fromIntegral p))
        ]
            ++ callC "indigo_push"
compilePushData (DTypeQuery q) = do
    lbl <- internString q
    return $
        [ Mov (Reg RDI) (Imm 10)
        , Lea (Reg RSI) (MemLabel lbl)
        ]
            ++ callC "indigo_push"
compilePushData (DMap m) = do
    let elems = Map.toList m
    let pushEntry (k, v) = do
            pk <- compilePushData (DString k)
            pv <- compilePushData v
            return (pk ++ pv)
    entries <- concatMapM pushEntry elems
    return $
        entries
            ++ [ Mov (Reg RDI) (Imm (length elems * 2))
               ]
            ++ callC "indigo_pack_map"
compilePushData (DFuncRef name args _) = do
    pArgs <- concatMapM compilePushData args
    tgt <- resolveTarget name
    nameLbl <- internString name
    return $
        pArgs
            ++ [ Lea (Reg RDI) (MemLabel (sanitizeLabel tgt))
               , Lea (Reg RSI) (MemLabel nameLbl)
               , Mov (Reg RDX) (Imm (length args))
               ]
            ++ callC "indigo_push_pf"

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs
