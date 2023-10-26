module AST where
import GHC.Generics (Generic)
import qualified Data.Binary

data Expr
    = Var String
    | BoolLit Bool
    | IntLit Integer
    | StringLit String
    | FloatLit Float
    | If Expr Expr Expr
    | Let String Expr
    | FuncDef {fname :: String, fargs :: [Expr], fbody :: Expr}
    | FuncCall String [Expr]
    | FuncDec {fname :: String, ftypes :: [Type]}
    | Function {fdef :: [Expr], fdec :: Expr}
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
    | InternalFunction {fname :: String, ifargs :: [Expr]}
    | Discard Expr
    | Import [String] String
    | Ref Expr
    | Struct {sname :: String, sfields :: [(String, Type)]}
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
    | Trait {tname :: String, tmethods :: [Expr]}
    | Impl {itrait :: String, ifor :: String, imethods :: [Expr]}
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
    show (Fn args ret) = "Fn{" ++ show args ++ " -> " ++ show ret ++ "}"
    show (List t) = "List{" ++ show t ++ "}"
    show (StructT name) = name
    show Self = "Self"

newtype Program = Program [Expr] deriving (Show, Eq, Generic)


instance Data.Binary.Binary Type

instance Data.Binary.Binary Expr

instance Data.Binary.Binary Program
