{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser (Expr (..), Program (..), Type (..), parseProgram) where

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
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
    ( MonadParsec (eof, takeWhile1P, try)
    , ParseErrorBundle
    , ParsecT
    , optional
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

data ParserState = ParserState
    { validFunctions :: [String]
    , validLets :: [String]
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
    | ModernFunc Expr Expr
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
    deriving
        ( Show
        )

data Type = Int | Bool | String | IO | Any | Fn {args :: [Type], ret :: Type} deriving (Show)

binOpTable :: [[Operator Parser Expr]]
binOpTable =
    [ [prefix "!" Not]
    , [binary "+" Add, binary "-" Sub]
    , [binary "*" Mul, binary "/" Div]
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
rws = ["if", "then", "else", "while", "do", "end", "true", "false", "not", "and", "or", "discard"]

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
        <?> "language"

stringLit :: Parser String
stringLit = char '\"' *> manyTill L.charLiteral (char '\"')

-- TODO: take a look at this in context of modern function syntax and consider making them consistent
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
    thenExpr <- expr
    symbol "else"
    If cond thenExpr <$> expr

doBlock :: Parser Expr
doBlock = do
    symbol "do"
    newline'
    exprs <- lines'
    symbol "end"
    return $ DoBlock exprs

modernFunction :: Parser Expr
modernFunction = do
    name <- identifier
    (args, argTypes) <-
        unzip <$> many do
            arg <- identifier
            symbol ":"
            argType <- validType
            optional $ symbol "->"
            return (arg, argType)

    symbol "=>"
    returnType <- validType <?> "return type"
    symbol "="

    state <- get
    put state{validFunctions = name : validFunctions state ++ [name | (name, x@(Fn _ _)) <- zip args argTypes], validLets = args}

    body <- expr
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

placeholder :: Parser Expr
placeholder = symbol "()" >> return Placeholder

newtype Program = Program [Expr] deriving (Show)

parseProgram' :: Text -> State ParserState (Either (ParseErrorBundle Text Void) Program)
parseProgram' = runParserT program ""

parseProgram :: Text -> Either (ParseErrorBundle Text Void) Program
parseProgram t = do
    let result = evalState (parseProgram' t) (ParserState [] [])
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

term :: Parser Expr
term =
    choice
        [ placeholder
        , parens expr
        , IntLit <$> try integer
        , StringLit <$> try stringLit
        , symbol "True" >> return (BoolLit True)
        , symbol "False" >> return (BoolLit False)
        , try import_
        , try externDec
        , doBlock
        , try letExpr
        , try modernFunction
        , try funcDec
        , try funcDef
        , try internalFunction
        , try discard
        , try funcCall
        , ifExpr
        , var
        , ref
        ]