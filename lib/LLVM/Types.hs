{-# LANGUAGE DeriveGeneric #-}

module LLVM.Types where

import Data.Int (Int64)
import GHC.Generics (Generic)

data LLVMType
    = I1
    | I8
    | I32
    | I64
    | FloatT
    | DoubleT
    | Ptr
    | Void
    | NamedType String
    | ArrayType Int LLVMType
    | StructType [LLVMType]
    deriving (Eq, Show, Generic)

data Operand
    = LocalVar String
    | GlobalVar String
    | ConstInt Int64
    | ConstFloat Float
    | ConstDouble Double
    | ConstBool Bool
    | ConstNull
    deriving (Eq, Show, Generic)

data LLVMInstr
    = AllocaOp LLVMType String
    | LoadOp LLVMType String Operand
    | StoreOp LLVMType Operand Operand
    | GetElementPtrOp LLVMType String Operand [Operand]
    | CallOp LLVMType (Maybe String) Operand [(LLVMType, Operand)]
    | AddOp LLVMType String Operand Operand
    | SubOp LLVMType String Operand Operand
    | MulOp LLVMType String Operand Operand
    | SDivOp LLVMType String Operand Operand
    | FAddOp String Operand Operand
    | FSubOp String Operand Operand
    | FMulOp String Operand Operand
    | FDivOp String Operand Operand
    | ICmpOp String LLVMType String Operand Operand
    | FCmpOp String String Operand Operand
    | BrOp String
    | BrCondOp Operand String String
    | RetOp (Maybe (LLVMType, Operand))
    | PtrToIntOp String Operand LLVMType
    | UnreachableOp
    | CommentLine String
    | RawIR String
    deriving (Eq, Show, Generic)

data BasicBlock = BasicBlock
    { blockLabel :: String
    , blockInstrs :: [LLVMInstr]
    }
    deriving (Eq, Show, Generic)

data Function = Function
    { fnRetType :: LLVMType
    , fnName :: String
    , fnParams :: [(LLVMType, String)]
    , fnBlocks :: [BasicBlock]
    }
    deriving (Eq, Show, Generic)

data GlobalDef
    = GlobalString String String Int
    | GlobalDecl LLVMType String [LLVMType]
    | GlobalVarDef LLVMType String
    deriving (Eq, Show, Generic)

data LLVMModule = LLVMModule
    { moduleName :: String
    , globals :: [GlobalDef]
    , functions :: [Function]
    }
    deriving (Eq, Show, Generic)
