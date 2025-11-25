module AST where

import Data.Binary qualified
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import GHC.Generics (Generic)

data GenericExpr = GenericExpr String (Maybe Type) deriving (Show, Eq, Generic, Ord)

data Expr
    = Var {varName :: String, varPos :: Position}
    | BoolLit {boolValue :: Bool, boolPos :: Position}
    | IntLit {intValue :: Integer, intPos :: Position}
    | StringLit {stringValue :: String, stringPos :: Position}
    | FloatLit {floatValue :: Float, floatPos :: Position}
    | DoubleLit {doubleValue :: Double, doublePos :: Position}
    | If {ifCond :: Expr, ifThen :: Expr, ifElse :: Expr, ifPos :: Position}
    | Let {letName :: String, letValue :: Expr, letPos :: Position}
    | FuncDef {name :: String, args :: [Expr], body :: Expr, funcDefPos :: Position}
    | FuncCall {funcName :: String, funcArgs :: [Expr], funcPos :: Position}
    | FuncDec {name :: String, types :: [Type], generics :: [GenericExpr], funcDecPos :: Position}
    | Function {def :: [Expr], dec :: Expr, functionPos :: Position}
    | DoBlock {doBlockExprs :: [Expr], doBlockPos :: Position}
    | ExternDec {externName :: String, externType :: String, externArgs :: [Type], externDecPos :: Position}
    | Add {addLhs :: Expr, addRhs :: Expr, addPos :: Position}
    | Sub {subLhs :: Expr, subRhs :: Expr, subPos :: Position}
    | Mul {mulLhs :: Expr, mulRhs :: Expr, mulPos :: Position}
    | Div {divLhs :: Expr, divRhs :: Expr, divPos :: Position}
    | Eq {eqLhs :: Expr, eqRhs :: Expr, eqPos :: Position}
    | Neq {neqLhs :: Expr, neqRhs :: Expr, neqPos :: Position}
    | Lt {ltLhs :: Expr, ltRhs :: Expr, ltPos :: Position}
    | Gt {gtLhs :: Expr, gtRhs :: Expr, gtPos :: Position}
    | Le {leLhs :: Expr, leRhs :: Expr, lePos :: Position}
    | Ge {geLhs :: Expr, geRhs :: Expr, gePos :: Position}
    | And {andLhs :: Expr, andRhs :: Expr, andPos :: Position}
    | Or {orLhs :: Expr, orRhs :: Expr, orPos :: Position}
    | Not {notExpr :: Expr, notPos :: Position}
    | UnaryMinus {unaryMinusExpr :: Expr, unaryMinusPos :: Position}
    | Placeholder {placeholderPos :: Position}
    | Discard {discardExpr :: Expr, discardPos :: Position}
    | Import {objects :: [String], from :: String, qualified :: Bool, as :: Maybe String, importPos :: Position}
    | Ref {refExpr :: Expr, refPos :: Position}
    | Struct {name :: String, fields :: [(String, Type)], refinement :: Maybe Expr, refinementSrc :: String, is :: [String], structPos :: Position}
    | StructLit {structLitName :: String, structLitFields :: [(String, Expr)], structLitPos :: Position}
    | StructAccess {structAccessStruct :: Expr, structAccessField :: Expr, structAccessPos :: Position}
    | ListLit {listLitExprs :: [Expr], listLitPos :: Position}
    | ListPattern {listPatternExprs :: [Expr], listPatternPos :: Position}
    | ListConcat {listConcatLhs :: Expr, listConcatRhs :: Expr, listConcatPos :: Position}
    | ListAdd {listAddLhs :: Expr, listAddRhs :: Expr, listAddPos :: Position}
    | ArrayAccess {arrayAccessArray :: Expr, arrayAccessIndex :: Expr, arrayAccessPos :: Position}
    | Modulo {moduloLhs :: Expr, moduloRhs :: Expr, moduloPos :: Position}
    | Power {powerBase :: Expr, powerExponent :: Expr, powerPos :: Position}
    | Target {targetName :: String, targetExpr :: Expr, targetPos :: Position}
    | Then {thenLhs :: Expr, thenRhs :: Expr, thenPos :: Position}
    | Pipeline {pipelineLhs :: Expr, pipelineRhs :: Expr, pipelinePos :: Position}
    | Lambda {lambdaArgs :: [Expr], lambdaBody :: Expr, lambdaPos :: Position}
    | Cast {castExpr :: Expr, castType :: Expr, castPos :: Position}
    | TypeLit {typeLitType :: Type, typeLitPos :: Position}
    | Flexible {flexibleExpr :: Expr, flexiblePos :: Position}
    | Trait {name :: String, methods :: [Expr], traitPos :: Position}
    | Impl {trait :: String, for :: String, methods :: [Expr], implPos :: Position}
    | StrictEval {strictEvalExpr :: Expr, strictEvalPos :: Position}
    | External {externalName :: String, externalArgs :: [Expr], externalPos :: Position}
    | CharLit {charValue :: Char, charPos :: Position}
    | ParenApply {parenApplyExpr :: Expr, parenApplyArgs :: [Expr], parenApplyPos :: Position}
    | When {whenExpr :: Expr, whenBranches :: [(Expr, Expr)], whenElse :: Maybe Expr, whenPos :: Position}
    | TupleLit {tupleLitExprs :: [Expr], tupleLitPos :: Position}
    | TupleAccess {tupleAccessTuple :: Expr, tupleAccessIndex :: Int, tupleAccessPos :: Position}
    deriving
        ( Show
        , Generic
        , Eq
        )

children :: Expr -> [Expr]
children (Add a b _) = [a, b]
children (Sub a b _) = [a, b]
children (Mul a b _) = [a, b]
children (Div a b _) = [a, b]
children (Eq a b _) = [a, b]
children (Neq a b _) = [a, b]
children (Lt a b _) = [a, b]
children (Gt a b _) = [a, b]
children (Le a b _) = [a, b]
children (Ge a b _) = [a, b]
children (And a b _) = [a, b]
children (Or a b _) = [a, b]
children (Not a _) = [a]
children (UnaryMinus a _) = [a]
children (If a b c _) = [a, b, c]
children (Let _ a _) = [a]
children (FuncDef{body}) = [body]
children (FuncCall{funcArgs}) = funcArgs
children (Function a b _) = a ++ [b]
children (DoBlock a _) = a
children (ExternDec{}) = []
children (Placeholder _) = []
children (Var _ _) = []
children (BoolLit _ _) = []
children (IntLit _ _) = []
children (StringLit _ _) = []
children (FloatLit _ _) = []
children (Discard a _) = [a]
children (Import{}) = []
children (Ref a _) = [a]
children (Struct{}) = []
children (StructLit{structLitFields}) = map snd structLitFields
children (StructAccess a b _) = [a, b]
children (ListLit a _) = a
children (ListPattern a _) = a
children (ListConcat a b _) = [a, b]
children (ArrayAccess a b _) = [a, b]
children (Modulo a b _) = [a, b]
children (Power a b _) = [a, b]
children (Target _ a _) = [a]
children (Then a b _) = [a, b]
children (Pipeline a b _) = [a, b]
children (Lambda _ a _) = [a]
children (Cast a b _) = [a, b]
children (TypeLit _ _) = []
children (Flexible a _) = [a]
children (Trait _ a _) = a
children (Impl _ _ a _) = a
children (FuncDec{}) = []
children (StrictEval a _) = [a]
children (External _ a _) = a
children (CharLit _ _) = []
children (DoubleLit _ _) = []
children (ParenApply a b _) = a : b
children (ListAdd a b _) = [a, b]
children (When expr branches else_ _) = expr : concatMap (\(p, b) -> [p, b]) branches ++ maybeToList else_
children (TupleLit a _) = a
children (TupleAccess a _ _) = [a]

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
    | Tuple [Type]
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
    show (Fn fnArgs fnRet) = "Fn (" ++ (if null fnArgs then "" else unwords $ fmap (++ " ->") (init $ fmap show fnArgs)) ++ (if null fnArgs then "" else " ") ++ show fnRet ++ ")"
    show (List t) = "[" ++ show t ++ "]"
    show (Tuple ts) = "(" ++ intercalate ", " (map show ts) ++ ")"
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
compareTypes (StructT x) (StructT y) = x == y
compareTypes (List x) (List y) = compareTypes x y
compareTypes (Tuple xs) (Tuple ys) = length xs == length ys && all (uncurry compareTypes) (zip xs ys)
compareTypes Unknown _ = True
compareTypes _ Unknown = True
compareTypes Any Any = True
compareTypes _ Any = True
compareTypes Any _ = False
compareTypes x y = x == y

typeOf :: Expr -> Type
typeOf (IntLit _ _) = StructT "Int"
typeOf (FloatLit _ _) = StructT "Float"
typeOf (BoolLit _ _) = StructT "Bool"
typeOf (StringLit _ _) = StructT "String"
typeOf (Add x _ _) = typeOf x
typeOf (Sub x _ _) = typeOf x
typeOf (Mul x _ _) = typeOf x
typeOf (Div x _ _) = typeOf x
typeOf (Power x _ _) = typeOf x
typeOf (UnaryMinus x _) = typeOf x
typeOf (Eq{}) = StructT "Bool"
typeOf (Neq{}) = StructT "Bool"
typeOf (Lt{}) = StructT "Bool"
typeOf (Gt{}) = StructT "Bool"
typeOf (Le{}) = StructT "Bool"
typeOf (Ge{}) = StructT "Bool"
typeOf (And{}) = StructT "Bool"
typeOf (Or{}) = StructT "Bool"
typeOf (Not _ _) = StructT "Bool"
typeOf (FuncCall{}) = Any
typeOf (Placeholder _) = Any
typeOf Var{} = error "This should never happen"
typeOf (Let{}) = error "Cannot infer type of let"
typeOf (If _ b _ _) = typeOf b
typeOf (FuncDef{}) = error "Cannot infer type of function definition"
typeOf x@(FuncDec{}) = error $ "Cannot infer type of function declaration " ++ show x
typeOf (Function{}) = Unknown -- error "Cannot infer type of modern function"
typeOf (DoBlock x _) = if null x then None else typeOf $ last x
typeOf (ExternDec{}) = error "Cannot infer type of extern declaration"
typeOf (Discard _ _) = error "Cannot infer type of discard"
typeOf (Import{}) = error "Cannot infer type of import"
typeOf (Ref _ _) = error "Cannot infer type of ref"
typeOf (Struct{}) = error "Cannot infer type of struct"
typeOf (StructLit x _ _) = StructT x
typeOf (ListLit [Var{varName}] _) = List $ StructT varName
typeOf (ListLit x _) = case x of
    [] -> List Any
    (y : _) -> List $ typeOf y
typeOf (ArrayAccess{}) = error "Cannot infer type of array access"
typeOf (Modulo x _ _) = typeOf x
typeOf (Target{}) = error "Cannot infer type of target"
typeOf (ListConcat x _ _) = typeOf x
typeOf (ListPattern _ _) = List Any
typeOf (StructAccess _ s _) = typeOf s
typeOf (Pipeline _ b _) = typeOf b
typeOf (Lambda{}) = Fn [] Any
typeOf (Cast _ (Var to _) _) = StructT to
typeOf (Cast _ b _) = typeOf b
typeOf (TypeLit x _) = x
typeOf (Flexible x _) = typeOf x
typeOf (Trait{}) = error "Cannot infer type of trait"
typeOf (Impl{}) = error "Cannot infer type of impl"
typeOf (Then _ b _) = typeOf b
typeOf (StrictEval x _) = typeOf x
typeOf (External{}) = error "Cannot infer type of external"
typeOf (CharLit _ _) = StructT "Char"
typeOf (DoubleLit _ _) = StructT "Double"
typeOf (ParenApply a _ _) = typeOf a
typeOf (ListAdd x _ _) = typeOf x
typeOf (When _ branches else_ _) = case branches of
    [] -> maybe Unknown typeOf else_
    ((_, body) : _) -> typeOf body
typeOf (TupleLit exprs _) = Tuple (map typeOf exprs)
typeOf (TupleAccess tupleExpr index _) = case typeOf tupleExpr of
    Tuple types -> if index >= 0 && index < length types then types !! index else Unknown
    _ -> Unknown

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
typeToString (Tuple ts) = "(" ++ intercalate ", " (map typeToString ts) ++ ")"
typeToString Self = "Self"
