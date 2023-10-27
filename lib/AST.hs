module AST where

import Data.Binary qualified
import GHC.Generics (Generic)

data Expr
    = Var String
    | BoolLit Bool
    | IntLit Integer
    | StringLit String
    | FloatLit Float
    | If Expr Expr Expr
    | Let String Expr
    | FuncDef {name :: String, args :: [Expr], body :: Expr}
    | FuncCall String [Expr]
    | FuncDec {name :: String, types :: [Type]}
    | Function {def :: [Expr], dec :: Expr}
    | DoBlock [Expr]
    | ExternDec String String [Type]
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Eq Expr Expr
    | Neq Expr Expr
    | Lt Expr Expr
    | Gt Expr Expr
    | Le Expr Expr
    | Ge Expr Expr
    | And Expr Expr
    | Or Expr Expr
    | Not Expr
    | UnaryMinus Expr
    | Placeholder
    | InternalFunction {name :: String, args :: [Expr]}
    | Discard Expr
    | Import [String] String
    | Ref Expr
    | Struct {name :: String, fields :: [(String, Type)]}
    | StructLit String [(String, Expr)]
    | StructAccess Expr Expr
    | ListLit [Expr]
    | ListPattern [Expr]
    | ListConcat Expr Expr
    | ArrayAccess Expr Expr
    | Modulo Expr Expr
    | Power Expr Expr
    | Target String Expr
    | Then Expr Expr
    | Bind Expr Expr
    | Lambda [Expr] Expr
    | Cast Expr Expr
    | TypeLit Type
    | Flexible Expr
    | Trait {name :: String, methods :: [Expr]}
    | Impl {trait :: String, for :: String, methods :: [Expr]}
    | IOLit
    deriving
        ( Show
        , Generic
        , Eq
        )

data Type
    = Int
    | Float
    | Bool
    | String
    | IO
    | Any
    | None
    | Unknown
    | Fn {args :: [Type], ret :: Type}
    | List Type
    | StructT String
    | Self
    deriving (Eq, Generic)

instance Show Type where
    show Int = "Int"
    show Float = "Float"
    show Bool = "Bool"
    show String = "String"
    show IO = "IO"
    show Any = "Any"
    show None = "None"
    show Unknown = "Unknown"
    show (Fn fnArgs fnRet) = "Fn{" ++ show fnArgs ++ " -> " ++ show fnRet ++ "}"
    show (List t) = "List{" ++ show t ++ "}"
    show (StructT structName) = structName
    show Self = "Self"

newtype Program = Program [Expr] deriving (Show, Eq, Generic)

instance Data.Binary.Binary Type

instance Data.Binary.Binary Expr

instance Data.Binary.Binary Program
