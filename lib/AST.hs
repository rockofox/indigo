module AST where

import Data.Binary qualified
import GHC.Generics (Generic)

data GenericExpr = GenericExpr String (Maybe Type) deriving (Show, Eq, Generic, Ord)

data Expr
    = Var String Position
    | BoolLit Bool
    | IntLit Integer
    | StringLit String
    | FloatLit Float
    | DoubleLit Double
    | If Expr Expr Expr
    | Let String Expr
    | FuncDef {name :: String, args :: [Expr], body :: Expr}
    | FuncCall String [Expr] Position
    | FuncDec {name :: String, types :: [Type], generics :: [GenericExpr]}
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
    | Import {objects :: [String], from :: String, qualified :: Bool, as :: Maybe String}
    | Ref Expr
    | Struct {name :: String, fields :: [(String, Type)], refinement :: Maybe Expr, refinementSrc :: String, is :: [String]}
    | StructLit String [(String, Expr)] Position
    | StructAccess Expr Expr
    | ListLit [Expr]
    | ListPattern [Expr]
    | ListConcat Expr Expr
    | ListAdd Expr Expr
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
    | StrictEval Expr
    | External String [Expr]
    | CharLit Char
    | ParenApply Expr [Expr] Position
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
children (Import{}) = []
children (Ref a) = [a]
children (Struct{}) = []
children (StructLit _ a _) = map snd a
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
children (FuncDec{}) = []
children (StrictEval a) = [a]
children (External _ a) = a
children (CharLit _) = []
children (DoubleLit _) = []
children (ParenApply a b _) = a : b
children (ListAdd a b) = [a, b]

newtype Position = Position (Int, Int) deriving (Show, Generic, Ord)

zeroPosition :: Position
zeroPosition = Position (0, 0)

position :: Int -> Int -> Position
position start end = Position (start, end)

anyPosition :: Position
anyPosition = Position (-1, -1)

instance Eq Position where
    (Position (-1, -1)) == (Position (-1, -1)) = True
    (Position (start1, end1)) == Position (-1, -1) = start1 >= 0 && end1 >= 0
    (Position (-1, -1)) == (Position (start2, end2)) = start2 >= 0 && end2 >= 0
    (Position (start1, end1)) == (Position (start2, end2)) = start1 == start2 && end1 == end2

data Type
    = Any
    | None
    | Unknown
    | Fn {args :: [Type], ret :: Type}
    | List Type
    | StructT String
    | Self
    deriving (Eq, Ord, Generic)

instance Show Type where
    show (StructT "Int") = "Int"
    show (StructT "Float") = "Float"
    show (StructT "Double") = "Double"
    show (StructT "Bool") = "Bool"
    show (StructT "String") = "String"
    show (StructT "CPtr") = "CPtr"
    show (StructT "Char") = "Char"
    show Any = "Any"
    show None = "None"
    show Unknown = "Unknown"
    show (Fn fnArgs fnRet) = "(" ++ (if null fnArgs then "" else unwords $ fmap (++ " ->") (init $ fmap show fnArgs)) ++ (if null fnArgs then "" else " ") ++ show fnRet ++ ")"
    show (List t) = "[" ++ show t ++ "]"
    show (StructT structName) = structName
    show Self = "Self"

newtype Program = Program {exprs :: [Expr]} deriving (Show, Eq, Generic)

instance Data.Binary.Binary Type

instance Data.Binary.Binary GenericExpr

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
compareTypes Unknown _ = True
compareTypes _ Unknown = True
compareTypes x y = x == y || x == Any || y == Any

typeOf :: Expr -> Type
typeOf (IntLit _) = StructT "Int"
typeOf (FloatLit _) = StructT "Float"
typeOf (BoolLit _) = StructT "Bool"
typeOf (StringLit _) = StructT "String"
typeOf (Add x _) = typeOf x
typeOf (Sub x _) = typeOf x
typeOf (Mul x _) = typeOf x
typeOf (Div x _) = typeOf x
typeOf (Power x _) = typeOf x
typeOf (UnaryMinus x) = typeOf x
typeOf (Eq _ _) = StructT "Bool"
typeOf (Neq _ _) = StructT "Bool"
typeOf (Lt _ _) = StructT "Bool"
typeOf (Gt _ _) = StructT "Bool"
typeOf (Le _ _) = StructT "Bool"
typeOf (Ge _ _) = StructT "Bool"
typeOf (And _ _) = StructT "Bool"
typeOf (Or _ _) = StructT "Bool"
typeOf (Not _) = StructT "Bool"
typeOf (FuncCall{}) = Any
typeOf Placeholder = Any
typeOf (Var{}) = Any
typeOf (Let _ _) = error "Cannot infer type of let"
typeOf (If _ b _) = typeOf b
typeOf (FuncDef{}) = error "Cannot infer type of function definition"
typeOf x@(FuncDec{}) = error $ "Cannot infer type of function declaration " ++ show x
typeOf (Function _ _) = Unknown -- error "Cannot infer type of modern function"
typeOf (DoBlock x) = if null x then None else typeOf $ last x
typeOf (ExternDec{}) = error "Cannot infer type of extern declaration"
typeOf (Discard _) = error "Cannot infer type of discard"
typeOf (Import{}) = error "Cannot infer type of import"
typeOf (Ref _) = error "Cannot infer type of ref"
typeOf (Struct{}) = error "Cannot infer type of struct"
typeOf (StructLit x _ _) = StructT x
typeOf (ListLit [Var x _]) = List $ StructT x
typeOf (ListLit x) = if null x then List Any else List $ typeOf $ head x
typeOf (ArrayAccess _ _) = error "Cannot infer type of array access"
typeOf (Modulo x _) = typeOf x
typeOf (Target _ _) = error "Cannot infer type of target"
typeOf (ListConcat x _) = typeOf x
typeOf (ListPattern _) = List Any
typeOf (StructAccess _ s) = typeOf s
typeOf (Pipeline _ b) = typeOf b
typeOf (Lambda _ _) = Fn [] Any
typeOf (Cast _ (Var to _)) = StructT to
typeOf (Cast _ b) = typeOf b
typeOf (TypeLit x) = x
typeOf (Flexible x) = typeOf x
typeOf (Trait _ _) = error "Cannot infer type of trait"
typeOf (Impl{}) = error "Cannot infer type of impl"
typeOf (Then _ b) = typeOf b
typeOf (StrictEval x) = typeOf x
typeOf (External _ _) = error "Cannot infer type of external"
typeOf (CharLit _) = StructT "Char"
typeOf (DoubleLit _) = StructT "Double"
typeOf (ParenApply a _ _) = typeOf a
typeOf (ListAdd x _) = typeOf x

typesMatch :: [Type] -> [Type] -> Bool
typesMatch [] [] = True
typesMatch (x : xs) (y : ys) = compareTypes x y && typesMatch xs ys
typesMatch _ _ = False

typeToString :: Type -> String
typeToString (StructT x) = x
typeToString Any = "Any"
typeToString None = "None"
typeToString Unknown = "Unknown"
typeToString (Fn args ret) = "Fn{" ++ show args ++ " -> " ++ show ret ++ "}"
typeToString (List t) = "List{" ++ typeToString t ++ "}"
typeToString Self = "Self"
