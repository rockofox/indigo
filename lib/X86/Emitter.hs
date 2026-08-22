{-# LANGUAGE LambdaCase #-}

module X86.Emitter (emitAssembly, emitInstruction, emitOperand) where

import Data.Char (toLower)
import X86.Sanitizer (escapeCString)
import X86.Types

emitAssembly :: AssemblyFile -> String
emitAssembly (AssemblyFile text rodata) =
    unlines $
        [ ".intel_syntax noprefix"
        , ".globl main"
        , ""
        , ".section .rodata"
        ]
            ++ concatMap emitRodata rodata
            ++ [ ""
               , ".section .text"
               ]
            ++ map emitInstruction text
            ++ [ ""
               , ".section .note.GNU-stack,\"\",@progbits"
               ]

emitRodata :: (String, String) -> [String]
emitRodata (lbl, str) =
    [ lbl ++ ":"
    , "    .string \"" ++ escapeCString str ++ "\""
    ]

emitInstruction :: X86Instr -> String
emitInstruction = \case
    LabelDef lbl -> lbl ++ ":"
    CommentLine c -> "    # " ++ c
    RawAsm s -> "    " ++ s
    Mov dst src -> "    mov " ++ emitOperand dst ++ ", " ++ emitOperand src
    Lea dst src -> "    lea " ++ emitOperand dst ++ ", " ++ emitOperand src
    PushOp op -> "    push " ++ emitOperand op
    PopOp op -> "    pop " ++ emitOperand op
    AddOp dst src -> "    add " ++ emitOperand dst ++ ", " ++ emitOperand src
    SubOp dst src -> "    sub " ++ emitOperand dst ++ ", " ++ emitOperand src
    MulOp op -> "    mul " ++ emitOperand op
    IdivOp op -> "    idiv " ++ emitOperand op
    AndOp dst src -> "    and " ++ emitOperand dst ++ ", " ++ emitOperand src
    OrOp dst src -> "    or " ++ emitOperand dst ++ ", " ++ emitOperand src
    XorOp dst src -> "    xor " ++ emitOperand dst ++ ", " ++ emitOperand src
    CmpOp a b -> "    cmp " ++ emitOperand a ++ ", " ++ emitOperand b
    TestOp a b -> "    test " ++ emitOperand a ++ ", " ++ emitOperand b
    SetE op -> "    sete " ++ emitOperand op
    SetNE op -> "    setne " ++ emitOperand op
    SetL op -> "    setl " ++ emitOperand op
    SetLE op -> "    setle " ++ emitOperand op
    SetG op -> "    setg " ++ emitOperand op
    SetGE op -> "    setge " ++ emitOperand op
    JmpOp lbl -> "    jmp " ++ lbl
    JzOp lbl -> "    jz " ++ lbl
    JnzOp lbl -> "    jnz " ++ lbl
    CallOp lbl -> "    call " ++ lbl
    RetOp -> "    ret"
    LeaveOp -> "    leave"

emitOperand :: Operand -> String
emitOperand = \case
    Reg r -> emitReg r
    Imm n -> show n
    ImmDouble _ -> error "ImmDouble should be lowered to bit integer before emission"
    Mem r 0 -> "[" ++ emitReg r ++ "]"
    Mem r offset
        | offset > 0 -> "[" ++ emitReg r ++ " + " ++ show offset ++ "]"
        | otherwise -> "[" ++ emitReg r ++ " - " ++ show (abs offset) ++ "]"
    MemLabel lbl -> "[rip + " ++ lbl ++ "]"
    LabelTarget lbl -> lbl
    StrConst s -> s

emitReg :: Reg64 -> String
emitReg = map toLower . show
