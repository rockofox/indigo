{-# LANGUAGE LambdaCase #-}

module LLVM.Emitter
    ( emitModule
    , emitInstruction
    , emitOperand
    , emitType
    )
where

import Data.List (intercalate)
import LLVM.Types

emitModule :: LLVMModule -> String
emitModule m =
    unlines $
        [ "; ModuleID = '" ++ moduleName m ++ "'"
        , "source_filename = \"" ++ moduleName m ++ "\""
        , ""
        , "%struct.IndigoValue = type { i64, i64 }"
        , ""
        , "@g_stack = external global [65536 x %struct.IndigoValue], align 16"
        , "@g_sp = external global i64, align 8"
        , "@g_sidestack = external global [65536 x %struct.IndigoValue], align 16"
        , "@g_ssp = external global i64, align 8"
        , "@g_current_frame = external global ptr, align 8"
        , "@g_memory = external global [65536 x %struct.IndigoValue], align 16"
        , ""
        ]
            ++ map emitGlobalDef runtimeDeclarations
            ++ [""]
            ++ map emitGlobalDef (globals m)
            ++ [""]
            ++ concatMap emitFunction (functions m)

runtimeDeclarations :: [GlobalDef]
runtimeDeclarations =
    [ GlobalDecl Void "indigo_push" [I64, I64]
    , GlobalDecl Void "indigo_dup" []
    , GlobalDecl Void "indigo_swp" []
    , GlobalDecl Void "indigo_enter_frame" []
    , GlobalDecl Void "indigo_enter_frame_inherit" []
    , GlobalDecl Void "indigo_leave_frame" []
    , GlobalDecl Void "indigo_storesidestack" []
    , GlobalDecl Void "indigo_loadsidestack" []
    , GlobalDecl Void "indigo_clearsidestack" []
    , GlobalDecl Void "indigo_lstore_slot" [I64]
    , GlobalDecl Void "indigo_lload_slot" [I64]
    , GlobalDecl Void "indigo_store" [I64]
    , GlobalDecl Void "indigo_load" [I64]
    , GlobalDecl Void "indigo_mov" [I64, I64, I64]
    , GlobalDecl Void "indigo_pack_list" [I64]
    , GlobalDecl Void "indigo_unpack_list" []
    , GlobalDecl Void "indigo_list_add" [I64]
    , GlobalDecl Void "indigo_concat" [I64]
    , GlobalDecl Void "indigo_index" []
    , GlobalDecl Void "indigo_slice" []
    , GlobalDecl Void "indigo_length" []
    , GlobalDecl Void "indigo_pack_map" [I64]
    , GlobalDecl Void "indigo_access" [Ptr]
    , GlobalDecl Void "indigo_aaccess" []
    , GlobalDecl Void "indigo_keys" []
    , GlobalDecl Void "indigo_update" []
    , GlobalDecl Void "indigo_push_pf" [Ptr, Ptr, I64]
    , GlobalDecl Ptr "indigo_calls_prepare" []
    , GlobalDecl Void "indigo_add" []
    , GlobalDecl Void "indigo_sub" []
    , GlobalDecl Void "indigo_mul" []
    , GlobalDecl Void "indigo_div" []
    , GlobalDecl Void "indigo_mod" []
    , GlobalDecl Void "indigo_pow" []
    , GlobalDecl Void "indigo_abs" []
    , GlobalDecl Void "indigo_eq" []
    , GlobalDecl Void "indigo_neq" []
    , GlobalDecl Void "indigo_lt" []
    , GlobalDecl Void "indigo_gt" []
    , GlobalDecl Void "indigo_not" []
    , GlobalDecl Void "indigo_and" []
    , GlobalDecl Void "indigo_or" []
    , GlobalDecl Void "indigo_cast" []
    , GlobalDecl Void "indigo_typeof" []
    , GlobalDecl Void "indigo_type_eq" []
    , GlobalDecl Void "indigo_exit" []
    , GlobalDecl Void "indigo_builtin_print" []
    , GlobalDecl Void "indigo_builtin_getline" []
    , GlobalDecl Void "indigo_builtin_getchar" []
    , GlobalDecl Void "indigo_builtin_random" []
    , GlobalDecl Void "indigo_label_not_found" [Ptr]
    ]

emitGlobalDef :: GlobalDef -> String
emitGlobalDef = \case
    GlobalString lbl escaped len ->
        "@" ++ lbl ++ " = private unnamed_addr constant [" ++ show len ++ " x i8] " ++ escaped ++ ", align 1"
    GlobalDecl retTy name paramTys ->
        "declare " ++ emitType retTy ++ " @" ++ name ++ "(" ++ intercalate ", " (map emitType paramTys) ++ ")"
    GlobalVarDef ty name ->
        "@" ++ name ++ " = external global " ++ emitType ty ++ ", align 8"

emitFunction :: Function -> [String]
emitFunction fn =
    [ "define "
        ++ emitType (fnRetType fn)
        ++ " @"
        ++ fnName fn
        ++ "("
        ++ intercalate ", " (map (\(t, n) -> emitType t ++ " %" ++ n) (fnParams fn))
        ++ ") {"
    ]
        ++ concatMap emitBasicBlock (fnBlocks fn)
        ++ ["}", ""]

emitBasicBlock :: BasicBlock -> [String]
emitBasicBlock bb =
    (blockLabel bb ++ ":")
        : map emitInstruction (blockInstrs bb)

emitInstruction :: LLVMInstr -> String
emitInstruction = \case
    AllocaOp ty dst -> "    %" ++ dst ++ " = alloca " ++ emitType ty
    LoadOp ty dst op -> "    %" ++ dst ++ " = load " ++ emitType ty ++ ", ptr " ++ emitOperand op
    StoreOp ty val ptrOp -> "    store " ++ emitType ty ++ " " ++ emitOperand val ++ ", ptr " ++ emitOperand ptrOp
    GetElementPtrOp ty dst base indices ->
        "    %" ++ dst ++ " = getelementptr " ++ emitType ty ++ ", ptr " ++ emitOperand base ++ concatMap (\i -> ", i64 " ++ emitOperand i) indices
    CallOp ty mDst fnOp args ->
        let callStr = "call " ++ emitType ty ++ " " ++ emitOperand fnOp ++ "(" ++ intercalate ", " (map (\(t, o) -> emitType t ++ " " ++ emitOperand o) args) ++ ")"
         in case mDst of
                Just dst -> "    %" ++ dst ++ " = " ++ callStr
                Nothing -> "    " ++ callStr
    AddOp ty dst a b -> "    %" ++ dst ++ " = add " ++ emitType ty ++ " " ++ emitOperand a ++ ", " ++ emitOperand b
    SubOp ty dst a b -> "    %" ++ dst ++ " = sub " ++ emitType ty ++ " " ++ emitOperand a ++ ", " ++ emitOperand b
    MulOp ty dst a b -> "    %" ++ dst ++ " = mul " ++ emitType ty ++ " " ++ emitOperand a ++ ", " ++ emitOperand b
    SDivOp ty dst a b -> "    %" ++ dst ++ " = sdiv " ++ emitType ty ++ " " ++ emitOperand a ++ ", " ++ emitOperand b
    FAddOp dst a b -> "    %" ++ dst ++ " = fadd float " ++ emitOperand a ++ ", " ++ emitOperand b
    FSubOp dst a b -> "    %" ++ dst ++ " = fsub float " ++ emitOperand a ++ ", " ++ emitOperand b
    FMulOp dst a b -> "    %" ++ dst ++ " = fmul float " ++ emitOperand a ++ ", " ++ emitOperand b
    FDivOp dst a b -> "    %" ++ dst ++ " = fdiv float " ++ emitOperand a ++ ", " ++ emitOperand b
    ICmpOp cond ty dst a b -> "    %" ++ dst ++ " = icmp " ++ cond ++ " " ++ emitType ty ++ " " ++ emitOperand a ++ ", " ++ emitOperand b
    FCmpOp cond dst a b -> "    %" ++ dst ++ " = fcmp " ++ cond ++ " float " ++ emitOperand a ++ ", " ++ emitOperand b
    BrOp lbl -> "    br label %" ++ lbl
    BrCondOp cond trueLbl falseLbl -> "    br i1 " ++ emitOperand cond ++ ", label %" ++ trueLbl ++ ", label %" ++ falseLbl
    RetOp Nothing -> "    ret void"
    RetOp (Just (ty, op)) -> "    ret " ++ emitType ty ++ " " ++ emitOperand op
    PtrToIntOp dst src toTy -> "    %" ++ dst ++ " = ptrtoint ptr " ++ emitOperand src ++ " to " ++ emitType toTy
    UnreachableOp -> "    unreachable"
    CommentLine c -> "    ; " ++ c
    RawIR s -> "    " ++ s

emitOperand :: Operand -> String
emitOperand = \case
    LocalVar s -> "%" ++ s
    GlobalVar s -> "@" ++ s
    ConstInt n -> show n
    ConstFloat f -> show f
    ConstDouble d -> show d
    ConstBool True -> "1"
    ConstBool False -> "0"
    ConstNull -> "null"

emitType :: LLVMType -> String
emitType = \case
    I1 -> "i1"
    I8 -> "i8"
    I32 -> "i32"
    I64 -> "i64"
    FloatT -> "float"
    DoubleT -> "double"
    Ptr -> "ptr"
    Void -> "void"
    NamedType s -> s
    ArrayType n ty -> "[" ++ show n ++ " x " ++ emitType ty ++ "]"
    StructType ts -> "{" ++ intercalate ", " (map emitType ts) ++ "}"
