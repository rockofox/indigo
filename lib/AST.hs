module AST where

import Control.Monad (liftM)
import Data.Bifunctor (second)
import Data.Binary qualified
import GHC.Generics (Generic)

data Expr
    = Var String Position
    | BoolLit Bool
    | IntLit Integer
    | StringLit String
    | FloatLit Float
    | If Expr Expr Expr
    | Let String Expr
    | FuncDef {name :: String, args :: [Expr], body :: Expr}
    | FuncCall String [Expr] Position
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
    | Pipeline Expr Expr
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

children :: Expr -> [Expr]
children (Add a b) = [a, b]
children (Sub a b) = [a, b]
children (Mul a b) = [a, b]
children (Div a b) = [a, b]
children (Eq a b) = [a, b]
children (Neq a b) = [a, b]
children (Lt a b) = [a, b]
children (Gt a b) = [a, b]
children (Le a b) = [a, b]
children (Ge a b) = [a, b]
children (And a b) = [a, b]
children (Or a b) = [a, b]
children (Not a) = [a]
children (UnaryMinus a) = [a]
children (If a b c) = [a, b, c]
children (Let _ a) = [a]
children (FuncDef _ _ a) = [a]
children (FuncCall _ a _) = a
children (Function a b) = a ++ [b]
children (DoBlock a) = a
children (ExternDec{}) = []
children Placeholder = []
children (Var _ _) = []
children (BoolLit _) = []
children (IntLit _) = []
children (StringLit _) = []
children (FloatLit _) = []
children (Discard a) = [a]
children (Import _ _) = []
children (Ref a) = [a]
children (Struct _ _) = []
children (StructLit _ a) = map snd a
children (StructAccess a b) = [a, b]
children (ListLit a) = a
children (ListPattern a) = a
children (ListConcat a b) = [a, b]
children (ArrayAccess a b) = [a, b]
children (Modulo a b) = [a, b]
children (Power a b) = [a, b]
children (Target _ a) = [a]
children (Then a b) = [a, b]
children (Pipeline a b) = [a, b]
children (Lambda _ a) = [a]
children (Cast a b) = [a, b]
children (TypeLit _) = []
children (Flexible a) = [a]
children (Trait _ a) = a
children (Impl _ _ a) = a
children IOLit = []
children (FuncDec _ _) = []

newtype Position = Position (Int, Int) deriving (Show, Generic, Ord)

zeroPosition :: Position
zeroPosition = Position (0, 0)

position :: Int -> Int -> Position
position start end = Position (start, end)

anyPosition :: Position
anyPosition = Position (-1, -1)

instance Eq Position where
    (Position (start1, end1)) == Position (-1, -1) = start1 >= 0 && end1 >= 0
    (Position (-1, -1)) == (Position (start2, end2)) = start2 >= 0 && end2 >= 0
    (Position (start1, end1)) == (Position (start2, end2)) = start1 == start2 && end1 == end2

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
    deriving (Eq, Ord, Generic)

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

instance Data.Binary.Binary Position

compareTypes :: Type -> Type -> Bool
compareTypes (Fn x y) (Fn a b) = do
    let argsMatch = all (uncurry compareTypes) $ zip x a
    let retMatch = compareTypes y b
    argsMatch && retMatch
compareTypes Self Self = True
compareTypes Self StructT{} = True
compareTypes StructT{} Self = True
compareTypes (List x) (List y) = compareTypes x y
compareTypes x y = x == y || x == Any || y == Any

typeOf :: Expr -> Type
typeOf (IntLit _) = Int
typeOf (FloatLit _) = Float
typeOf (BoolLit _) = Bool
typeOf (StringLit _) = String
typeOf (Add x _) = typeOf x
typeOf (Sub x _) = typeOf x
typeOf (Mul x _) = typeOf x
typeOf (Div x _) = typeOf x
typeOf (Power x _) = typeOf x
typeOf (UnaryMinus x) = typeOf x
typeOf (Eq _ _) = Bool
typeOf (Neq _ _) = Bool
typeOf (Lt _ _) = Bool
typeOf (Gt _ _) = Bool
typeOf (Le _ _) = Bool
typeOf (Ge _ _) = Bool
typeOf (And _ _) = Bool
typeOf (Or _ _) = Bool
typeOf (Not _) = Bool
typeOf (FuncCall{}) = error "Cannot infer type of function call"
typeOf Placeholder = None
typeOf (Var{}) = Unknown -- error "Cannot infer type of variable"
typeOf (Let _ _) = error "Cannot infer type of let"
typeOf (If{}) = error "Cannot infer type of if"
typeOf (FuncDef{}) = error "Cannot infer type of function definition"
typeOf (FuncDec _ _) = error "Cannot infer type of function declaration"
typeOf (Function _ _) = error "Cannot infer type of modern function"
typeOf (DoBlock _) = error "Cannot infer type of do block"
typeOf (ExternDec{}) = error "Cannot infer type of extern declaration"
typeOf (Discard _) = error "Cannot infer type of discard"
typeOf (Import _ _) = error "Cannot infer type of import"
typeOf (Ref _) = error "Cannot infer type of ref"
typeOf (Struct _ _) = error "Cannot infer type of struct"
typeOf (StructLit x _) = StructT x
typeOf (ListLit x) = if null x then List Any else List $ typeOf $ head x
typeOf (ArrayAccess _ _) = error "Cannot infer type of array access"
typeOf (Modulo _ _) = error "Cannot infer type of modulo"
typeOf (Target _ _) = error "Cannot infer type of target"
typeOf IOLit = IO
typeOf (ListConcat{}) = List Any
typeOf (ListPattern _) = List Any
typeOf (StructAccess s _) = typeOf s
typeOf (Pipeline _ b) = typeOf b
typeOf (Lambda _ _) = Fn [] Unknown
typeOf (Cast _ to) = typeOf to
typeOf (TypeLit x) = x
typeOf (Flexible x) = typeOf x
typeOf (Trait _ _) = error "Cannot infer type of trait"
typeOf (Impl{}) = error "Cannot infer type of impl"

-- typeOf x = error $ "Cannot infer type of " ++ show x

typesMatch :: [Type] -> [Type] -> Bool
typesMatch [] [] = True
typesMatch (x : xs) (y : ys) = compareTypes x y && typesMatch xs ys
typesMatch _ _ = False
