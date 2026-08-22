{-# LANGUAGE DeriveGeneric #-}

module X86.Types where

import GHC.Generics (Generic)

data Reg64
    = RAX
    | RBX
    | RCX
    | RDX
    | RSI
    | RDI
    | RBP
    | RSP
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    deriving (Eq, Show, Generic)

data Operand
    = Reg Reg64
    | Imm Int
    | ImmDouble Double
    | Mem Reg64 Int
    | MemLabel String
    | LabelTarget String
    | StrConst String
    deriving (Eq, Show, Generic)

data X86Instr
    = Mov Operand Operand
    | Lea Operand Operand
    | PushOp Operand
    | PopOp Operand
    | AddOp Operand Operand
    | SubOp Operand Operand
    | MulOp Operand
    | IdivOp Operand
    | AndOp Operand Operand
    | OrOp Operand Operand
    | XorOp Operand Operand
    | CmpOp Operand Operand
    | TestOp Operand Operand
    | SetE Operand
    | SetNE Operand
    | SetL Operand
    | SetLE Operand
    | SetG Operand
    | SetGE Operand
    | JmpOp String
    | JzOp String
    | JnzOp String
    | CallOp String
    | RetOp
    | LeaveOp
    | LabelDef String
    | CommentLine String
    | RawAsm String
    deriving (Eq, Show, Generic)

data AssemblyFile = AssemblyFile
    { textSection :: [X86Instr]
    , rodataStrings :: [(String, String)]
    }
    deriving (Eq, Show, Generic)
