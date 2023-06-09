{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser (Expr (..), Program (..), Type (..), parseProgram, CompilerFlags (..), typeOf) where

import Control.Monad.Combinators
    ( between
    , choice
    , many
    , manyTill
    , sepBy
    , sepBy1
    , sepEndBy
    , (<|>)
    )
import Control.Monad.Combinators.Expr (Operator (InfixL, Postfix, Prefix), makeExprParser)
import Control.Monad.State hiding (state)
import Data.Binary qualified
import Data.Text (Text, unpack)
import Data.Void (Void)
import GHC.Arr (indices)
import GHC.Generics (Generic)
import Text.Megaparsec
    ( MonadParsec (eof, lookAhead, notFollowedBy, takeWhile1P, try, withRecovery)
    , ParseError
    , ParseErrorBundle
    , ParsecT
    , optional
    , registerParseError
    , runParserT
    , (<?>)
    )
import Text.Megaparsec.Char
    ( alphaNumChar
    , char
    , letterChar
    , newline
    , space1
    )
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug

data CompilerFlags = CompilerFlags
    { verboseMode :: Bool
    }
    deriving
        ( Show
        )

data ParserState = ParserState
    { validFunctions :: [String]
    , validLets :: [String]
    , compilerFlags :: CompilerFlags
    }
    deriving
        ( Show
        )

type Parser = ParsecT Void Text (State ParserState)

data Expr
    = Var String
    | BoolLit Bool
    | IntLit Integer
    | StringLit String
    | BinOp String Expr Expr
    | If Expr Expr Expr
    | Let String Expr
    | FuncDef {fname :: String, fargs :: [String], fbody :: Expr}
    | FuncCall String [Expr]
    | FuncDec {fname :: String, ftypes :: [Type]}
    | ModernFunc {fdef :: Expr, fdec :: Expr}
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
    | Placeholder
    | InternalFunction {fname :: String, ifargs :: [Expr]}
    | Discard Expr
    | Import [String] String
    | Ref Expr
    | Struct String [(String, Type)]
    | StructLit String [(String, Expr)]
    | Array [Expr]
    | ArrayAccess Expr Expr
    | Modulo Expr Expr
    | Target String Expr
    deriving
        ( Show
        , Generic
        )

data Type = Int | Bool | String | IO | Any | None | Fn {args :: [Type], ret :: Type} deriving (Show, Eq, Generic)

newtype Program = Program [Expr] deriving (Show, Generic)

instance Data.Binary.Binary Type

instance Data.Binary.Binary Expr

instance Data.Binary.Binary Program

typeOf :: Expr -> Parser.Type
typeOf (IntLit _) = Int
typeOf (BoolLit _) = Bool
typeOf (StringLit _) = String
typeOf (Add x y) = typeOf x
typeOf (Sub x y) = typeOf x
typeOf (Mul x y) = typeOf x
typeOf (Div x y) = typeOf x
typeOf (Eq x y) = Bool
typeOf (Neq x y) = Bool
typeOf (Lt x y) = Bool
typeOf (Gt x y) = Bool
typeOf (Le x y) = Bool
typeOf (Ge x y) = Bool
typeOf (And x y) = Bool
typeOf (Or x y) = Bool
typeOf (Not x) = Bool
typeOf Placeholder = None
typeOf _ = Any

binOpTable :: [[Operator Parser Expr]]
binOpTable =
    [ [prefix "!" Not]
    , [binary "+" Add, binary "-" Sub]
    , [binary "*" Mul, binary "/" Div]
    , [binary "%" Modulo]
    , [binary "~>" And]
    , [binary "==" Eq, binary "!=" Neq, binary "<" Lt, binary ">" Gt, binary "<=" Le, binary ">=" Ge]
    , [binary "&&" And, binary "||" Or]
    ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

ref :: Parser Expr
ref = do
    symbol "*"
    Ref <$> funcCall

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

curlyBrackets :: Parser a -> Parser a
curlyBrackets = between (symbol "{") (symbol "}")

lineComment, blockComment :: Parser ()
lineComment = L.skipLineComment "#"
blockComment = L.skipBlockComment "/*" "*/"

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) lineComment blockComment
  where
    f x = x == ' ' || x == '\t'

scn :: Parser ()
scn = L.space space1 lineComment blockComment

newline' :: Parser ()
newline' = newline >> scn

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

rws :: [String] -- list of reserved words
rws = ["if", "then", "else", "while", "do", "end", "true", "false", "not", "and", "or", "discard", "let"]

identifier :: Parser String
identifier = do
    -- alphanumeric identifier, but not a reserved word. Also must start with a letter
    name <- (lexeme . try) (p >>= check)
    if name `elem` rws
        then fail $ "keyword " ++ show name ++ " cannot be an identifier"
        else return name
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
        if x `elem` rws
            then fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else return x

integer :: Parser Integer
integer = lexeme L.decimal

foreignIdentifier :: Parser String
foreignIdentifier = do
    -- identifier, plus _ and .
    name <- (lexeme . try) (p >>= check)
    if name `elem` rws
        then fail $ "keyword " ++ show name ++ " cannot be an identifier"
        else return name
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_' <|> char '.')
    check x =
        if x `elem` rws
            then fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else return x

expr :: Parser Expr
expr = makeExprParser term binOpTable

validType :: Parser Type
validType =
    do
        symbol "Int"
        return Int
        <|> do
            symbol "Bool"
            return Bool
        <|> do
            symbol "String"
            return String
        <|> do
            symbol "IO"
            return IO
        <|> do
            symbol "Any"
            return Any
        <|> do
            symbol "Fn"
            curlyBrackets $ do
                args <- sepBy validType (symbol "->")
                symbol "=>"
                ret <- validType
                return Fn{args = args, ret = ret}
        <?> "type"

externLanguage :: Parser String
externLanguage =
    do
        symbol "wasi_unstable"
        return "wasi_unstable"
        <|> do
            symbol "js"
            return "js"
        <|> do
            symbol "jsrev"
            return "jsrev"
        <|> do
            symbol "c"
            return "c"
        <|> do
            symbol "runtime"
            return "runtime"
        <?> "language"

stringLit :: Parser String
stringLit = char '\"' *> manyTill L.charLiteral (char '\"')

externDec :: Parser Expr
externDec = do
    symbol "extern"
    lang <- externLanguage
    name <- foreignIdentifier
    symbol "::"
    args <- sepBy validType (symbol "->")
    symbol "=>"
    ret <- validType
    state <- get
    put state{validFunctions = name : validFunctions state}

    return $ ExternDec lang name (args ++ [ret])

funcDec :: Parser Expr
funcDec = do
    name <- identifier
    symbol "::"
    argTypes <- sepBy1 validType (symbol "->")
    return $ FuncDec name argTypes

funcDef :: Parser Expr
funcDef = do
    name <- identifier <?> "function name"
    args <- many identifier <?> "function arguments"
    symbol "="
    FuncDef name args <$> expr <?> "function body"

funcCall :: Parser Expr
funcCall = do
    state <- get
    name <- foreignIdentifier <?> "function name"
    unless (name `elem` validFunctions state) $ fail $ "function " ++ name ++ " is not defined"
    args <- sepBy1 expr (symbol ",") <?> "function arguments"
    return $ FuncCall name args

internalFunctionName :: Parser String
internalFunctionName =
    do
        symbol "__wasm_i32_store"
        return "__wasm_i32_store"
        <|> do
            symbol "__wasm_i32_load"
            return "__wasm_i32_load"
        <?> "internal function name"

internalFunction :: Parser Expr
internalFunction = do
    name <- internalFunctionName
    args <- sepBy1 expr (symbol ",") <?> "function arguments"
    return $ InternalFunction name args

letExpr :: Parser Expr
letExpr = do
    symbol "let"
    name <- identifier
    symbol "="
    value <- expr
    state <- get
    put state{validLets = name : validLets state}
    return $ Let name value

ifExpr :: Parser Expr
ifExpr = do
    symbol "if"
    cond <- expr
    symbol "then"
    optional newline'
    thenExpr <- expr
    optional newline'
    symbol "else"
    optional newline'
    elseExpr <- expr
    optional newline'
    return $ If cond thenExpr elseExpr

doBlock :: Parser Expr
doBlock = do
    symbol "do"
    newline'
    exprs <- lines'
    symbol "end" <|> lookAhead (symbol "else")
    return $ DoBlock exprs

modernFunction :: Parser Expr
modernFunction = do
    name <- identifier <?> "function name"
    (args, argTypes) <-
        unzip <$> many do
            arg <- identifier <?> "function argument"
            symbol ":"
            argType <- validType <?> "function argument type"
            optional $ symbol "->"
            return (arg, argType) <?> "function arguments"
    symbol "=>"
    returnType <- validType <?> "return type"
    symbol "="

    state <- get
    put state{validFunctions = name : validFunctions state ++ [name | (name, x@(Fn _ _)) <- zip args argTypes], validLets = args}

    body <- recover expr <?> "function body"
    return $ ModernFunc (FuncDef name args body) (FuncDec name (returnType : argTypes))

discard :: Parser Expr
discard = do
    symbol "discard"
    Discard <$> expr

import_ :: Parser Expr
import_ = do
    symbol "import"
    objects <- sepBy identifier (symbol ",")
    symbol "from"
    Import objects <$> stringLit

array :: Parser Expr
array = do
    symbol "["
    elements <- sepBy expr (symbol ",")
    symbol "]"
    return $ Array elements

struct :: Parser Expr
struct = do
    symbol "struct"
    name <- identifier
    symbol "{"
    optional newline'
    fields <-
        sepBy1
            ( do
                fieldName <- identifier
                symbol ":"
                fieldType <- validType
                return (fieldName, fieldType)
            )
            (symbol "," <* optional newline')
    optional newline'
    symbol "}"
    return $ Struct name fields

structLit :: Parser Expr
structLit = do
    name <- identifier
    symbol "{"
    fields <-
        sepBy1
            ( do
                fieldName <- identifier
                symbol ":"
                fieldValue <- expr
                return (fieldName, fieldValue)
            )
            (symbol ",")
    symbol "}"
    return $ StructLit name fields

arrayAccess :: Parser Expr
arrayAccess = do
    a <- choice [var, array]
    symbol "["
    index <- expr
    symbol "]"
    return $ ArrayAccess a index

placeholder :: Parser Expr
placeholder = symbol "()" >> return Placeholder

parseProgram' :: Text -> State ParserState (Either (ParseErrorBundle Text Void) Program)
parseProgram' = runParserT program ""

parseProgram :: Text -> CompilerFlags -> Either (ParseErrorBundle Text Void) Program
parseProgram t cf = do
    let result = evalState (parseProgram' t) (ParserState{compilerFlags = cf, validLets = [], validFunctions = []})
    case result of
        Left err -> Left err
        Right program -> Right program

lines' :: Parser [Expr]
lines' = expr `sepEndBy` newline'

program :: Parser Program
program = Program <$> between scn eof lines'

var :: Parser Expr
var = do
    state <- get
    name <- identifier
    unless (name `elem` validLets state) $ fail $ "variable " ++ name ++ " is not defined. State: " ++ show state
    return $ Var name

target :: Parser Expr
target = do
    symbol "__target"
    target <- (symbol "wasm" <|> symbol "c") <?> "target"
    Target (unpack target) <$> expr

verbose :: Show a => String -> Parser a -> Parser a
verbose str parser = do
    state <- get
    let shouldBeVerbose = verboseMode $ compilerFlags state
    if shouldBeVerbose then dbg str parser else parser

recover :: Parser Expr -> Parser Expr
recover = withRecovery recover'
  where
    recover' :: ParseError Text Void -> Parser Expr
    recover' err = do
        registerParseError err
        return $ Placeholder

term :: Parser Expr
term =
    choice $
        ( (\(index, parser) -> verbose ("term " ++ show index) parser)
            <$> zip
                [0 ..]
                [ placeholder -- 0
                , parens expr -- 1
                , IntLit <$> try integer -- 2
                , StringLit <$> try stringLit -- 3
                , symbol "True" >> return (BoolLit True) -- 4
                , symbol "False" >> return (BoolLit False) -- 5
                , letExpr -- 6
                , import_ -- 7
                , externDec -- 8
                , doBlock -- 9
                , try modernFunction -- 10
                , target -- aw hell nah, screw it
                , struct -- 11
                , try structLit -- 12
                , array -- 13
                , -- , try funcDec
                  -- , try funcDef
                  try internalFunction -- 14
                , try discard -- 15
                , try funcCall -- 16
                , try arrayAccess -- 17
                , ifExpr -- 18
                , var -- 19
                , ref -- 20
                ]
        )