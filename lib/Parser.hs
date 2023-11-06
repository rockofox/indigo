{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parser (Expr (..), Program (..), Type (..), parseProgram, parseFreeUnsafe, parseAndVerify, CompilerFlags (..), typeOf, compareTypes) where

import AST
import Control.Monad qualified
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
import Control.Monad.State
    ( MonadState (get, put)
    , State
    , modify
    , runState
    )
import Data.Char (isUpper)
import Data.Text (Text, unpack)
import Data.Void (Void)
import Text.Megaparsec
    ( MonadParsec (eof, lookAhead, notFollowedBy, takeWhile1P, try, withRecovery)
    , ParseError (..)
    , ParseErrorBundle (..)
    , ParsecT
    , getOffset
    , oneOf
    , optional
    , registerParseError
    , runParserT
    , satisfy
    , some
    , (<?>)
    )
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char
    ( alphaNumChar
    , char
    , letterChar
    , newline
    , space1
    )
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug (dbg)
import Verifier (verifyProgram)

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
    , verifierTree :: [PosExpr]
    }
    deriving
        ( Show
        )

type Parser = ParsecT Void Text (State ParserState)

binOpTable :: [[Operator Parser Expr]]
binOpTable =
    [ [prefix "^" Flexible]
    , [binary "**" Power, binary "*" Mul, binary "/" Div]
    , [binary "%" Modulo]
    , [binary "+" Add, binary "-" Sub]
    , [binary ">>" Then]
    , [binary "~>" Bind]
    , [binary "." StructAccess]
    , [binary "as" Cast]
    , [binary ":" ListConcat]
    , [binary "==" Eq, binary "!=" Neq, binary "<" Lt, binary ">" Gt, binary "<=" Le, binary ">=" Ge]
    , [binary "&&" And, binary "||" Or]
    , [prefix "!" Not]
    , [prefix "-" UnaryMinus]
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
sc = L.space (Control.Monad.void (takeWhile1P Nothing f)) lineComment blockComment
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

keyword :: Text -> Parser ()
keyword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rws :: [String]
rws = ["if", "then", "else", "do", "end", "True", "False", "let", "as"]

identifier :: Parser String
identifier = do
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

float :: Parser Float
float = lexeme L.float

extra :: Parser String
extra = do
    name <- (lexeme . try) (p >>= check)
    if name `elem` rws
        then fail $ "keyword " ++ show name ++ " cannot be an identifier"
        else return name
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x =
        if x `elem` rws
            then fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else return x

expr :: Parser Expr
expr = do
    start <- getOffset
    ex <- makeExprParser term binOpTable
    end <- getOffset
    modify (\state -> state{verifierTree = PosExpr (ex, start, end) : verifierTree state})
    return ex

validType :: Parser Type
validType =
    do
        keyword "Int"
        return Int
        <|> do
            keyword "Float"
            return Float
        <|> do
            keyword "Bool"
            return Bool
        <|> do
            keyword "String"
            return String
        <|> do
            keyword "IO"
            return IO
        <|> do
            keyword "Any"
            return Any
        <|> do
            keyword "Fn"
            curlyBrackets $ do
                args <- sepBy validType (symbol "->")
                symbol "=>"
                ret <- validType
                return Fn{args = args, ret = ret}
        <|> do
            keyword "List"
            curlyBrackets $ List <$> validType
        <|> do
            keyword "Self"
            return Self
        <|> do
            lookAhead $ satisfy isUpper
            StructT <$> identifier
        <?> "type"

stringLit :: Parser String
stringLit = lexeme $ char '\"' *> manyTill L.charLiteral (char '\"')

funcDec :: Parser Expr
funcDec = do
    name <- identifier <|> gravis
    symbol "::"
    argTypes <- sepBy1 validType (symbol "->")
    return $ FuncDec name argTypes

funcDef :: Parser Expr
funcDef = do
    name <- identifier <|> gravis <?> "function name"
    args <- some (var <|> parens listPattern <|> array <|> placeholder) <?> "function arguments"
    symbol "="
    FuncDef name args <$> expr <?> "function body"

gravis :: Parser String
gravis = do
    symbol "`"
    name <- (: []) <$> oneOf ("+-/*" :: String)
    symbol "`"
    return name

funcCall :: Parser Expr
funcCall = do
    name <- (extra <|> gravis) <?> "function name"
    args <- sepBy1 expr (symbol ",") <?> "function arguments"
    return $ FuncCall name args

letExpr :: Parser Expr
letExpr = do
    keyword "let"
    name <- identifier
    symbol "="
    value <- recover expr
    state <- get
    put state{validLets = name : validLets state}
    return $ Let name value

ifExpr :: Parser Expr
ifExpr = do
    keyword "if"
    cond <- expr
    keyword "then"
    optional newline'
    thenExpr <- expr
    optional newline'
    keyword "else"
    optional newline'
    elseExpr <- expr
    optional newline'
    return $ If cond thenExpr elseExpr

doBlock :: Parser Expr
doBlock = do
    keyword "do"
    newline'
    exprs <- lines'
    keyword "end" <|> lookAhead (keyword "else")
    return $ DoBlock exprs

combinedFunc :: Parser Expr
combinedFunc = do
    name <- identifier <?> "function name"
    (args, argTypes) <-
        unzip <$> many do
            arg <- identifier <?> "function argument"
            symbol ":"
            argType <- validType <?> "function argument type"
            optional $ symbol "->"
            return (Var arg, argType) <?> "function arguments"
    symbol "=>"
    returnType <- validType <?> "return type"
    symbol "="

    -- state <- get
    -- put state{validFunctions = name : validFunctions state ++ [name | (name, x@(Fn _ _)) <- zip args argTypes], validLets = args}

    body <- recover expr <?> "function body"
    return $ Function [FuncDef name args body] (FuncDec name (argTypes ++ [returnType]))

import_ :: Parser Expr
import_ = do
    keyword "import"
    objects <- sepBy (extra <|> (symbol "*" >> return "*")) (symbol ",")
    keyword "from"
    from <- many (alphaNumChar <|> char '.') <?> "import path"
    return $ Import objects from

array :: Parser Expr
array = do
    symbol "["
    elements <- sepBy expr (symbol ",")
    symbol "]"
    return $ ListLit elements

struct :: Parser Expr
struct = do
    keyword "struct"
    name <- extra
    symbol "="
    -- traceShowM $ "Parsing struct " ++ show name
    -- fields <-
    --     parens (
    --         sepBy
    --             structField
    --             (symbol "," <* optional newline')
    --             )
    fields <- parens $ structField `sepBy` symbol ","
    -- traceShowM $ "Parsed struct " ++ show fields
    return $ Struct name fields
  where
    structField = do
        fieldName <- identifier <?> "field name"
        symbol ":"
        fieldType <- validType <?> "field type"
        return (fieldName, fieldType)

structLit :: Parser Expr
structLit = do
    name <- extra
    symbol "{"
    fields <-
        sepBy
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
placeholder = symbol "_" >> return Placeholder

parseProgram' :: Text -> State ParserState (Either (ParseErrorBundle Text Void) Program)
parseProgram' = runParserT program ""

parseProgram :: Text -> CompilerFlags -> Either (ParseErrorBundle Text Void) Program
parseProgram t cf = do
    let (result, _) = runState (parseProgram' t) (ParserState{compilerFlags = cf, validLets = [], validFunctions = [], verifierTree = []})
    case result of
        Left err -> Left err
        Right program' -> Right program'

parseAndVerify :: String -> Text -> CompilerFlags -> Either (ParseErrorBundle Text Void) Program
parseAndVerify name t cf = do
    let (result, state) = runState (parseProgram' t) (ParserState{compilerFlags = cf, validLets = [], validFunctions = [], verifierTree = []})
    case result of
        Left err -> Left err
        Right program' -> do
            case verifyProgram name t (verifierTree state) of
                Left err -> Left err
                Right _ -> Right program'

parseFreeUnsafe :: Text -> Expr
parseFreeUnsafe t = case parseProgram t (CompilerFlags{verboseMode = False}) of
    Left err -> error $ show err
    Right program' -> case program' of
        Program [exprs] -> exprs
        _ -> error "Program should only have one expression"

lines' :: Parser [Expr]
lines' = expr `sepEndBy` newline'

program :: Parser Program
program = Program <$> between scn eof lines'

var :: Parser Expr
var = Var <$> (extra <|> gravis)

target :: Parser Expr
target = do
    symbol "__target"
    target' <- (symbol "wasm" <|> symbol "c") <?> "target"
    Target (unpack target') <$> expr

listPattern :: Parser Expr
listPattern = do
    -- elements <- sepBy1 (identifier <|> (symbol "_" >> return "")) (symbol ":")
    elements <- sepBy1 (var <|> placeholder <|> array) (symbol ":")
    return $ ListPattern elements

lambda :: Parser Expr
lambda = do
    symbol "\\"
    args <- some (var <|> parens listPattern <|> array)
    symbol "->"
    Lambda args <$> expr

verbose :: (Show a) => String -> Parser a -> Parser a
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
        return Placeholder

typeLiteral :: Parser Expr
typeLiteral = TypeLit <$> validType

trait :: Parser Expr
trait = do
    symbol "trait"
    name <- identifier
    symbol "="
    symbol "do"
    newline'
    methods <- funcDec `sepEndBy` newline'
    symbol "end"
    return $ Trait name methods

impl :: Parser Expr
impl = do
    symbol "impl"
    name <- identifier
    symbol "for"
    for <- identifier
    symbol "="
    symbol "do"
    newline'
    methods <- funcDef `sepEndBy` newline'
    symbol "end"
    return $ Impl name for methods

term :: Parser Expr
term =
    choice
        [ placeholder
        , parens expr
        , FloatLit <$> try float
        , IntLit <$> try integer
        , StringLit <$> try stringLit
        , symbol "True" >> return (BoolLit True)
        , symbol "False" >> return (BoolLit False)
        , letExpr
        , struct
        , import_
        , doBlock
        , impl
        , Parser.trait
        , try combinedFunc
        , try funcDef
        , try funcDec
        , try lambda
        , try structLit
        , typeLiteral
        , array
        , try funcCall
        , try arrayAccess
        , ifExpr
        , var
        ]
