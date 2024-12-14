{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parser (Expr (..), Program (..), parseProgram', parseProgramPure, ParserState (..), Type (..), parseProgram, parseFreeUnsafe, CompilerFlags (..), initCompilerFlags, typeOf, compareTypes) where

import AST
import Control.Applicative.Combinators (someTill)
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
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.State
    ( MonadState (get, put)
    , State
    , runState
    )
import Data.Char (isUpper)
import Data.List
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Void (Void)
import GHC.Char (chr)
import Text.Megaparsec
    ( MonadParsec (eof, lookAhead, notFollowedBy, takeWhile1P, try, withRecovery)
    , ParseError (..)
    , ParseErrorBundle (..)
    , ParsecT
    , getOffset
    , noneOf
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

data CompilerFlags = CompilerFlags
    { verboseMode :: Bool
    , needsMain :: Bool
    }
    deriving
        ( Show
        )

initCompilerFlags :: CompilerFlags
initCompilerFlags = CompilerFlags{verboseMode = False, needsMain = True}

data ParserState = ParserState
    { validFunctions :: [String]
    , validLets :: [String]
    , compilerFlags :: CompilerFlags
    }
    deriving
        ( Show
        )

type Parser = ParsecT Void Text (State ParserState)

binOpTable :: [[Operator Parser Expr]]
binOpTable =
    [ [prefix "^" Flexible]
    , [binary "." StructAccess]
    , [binary "as" Cast]
    , [prefix "$" StrictEval]
    , [prefix "!" Not]
    , [binaryAny binaryFunctionCall]
    , -- , [prefix "-" UnaryMinus]
      [binary "**" Power, binary "*" Mul, binary "/" Div]
    , [binary "%" Modulo]
    , [binary ":" ListConcat]
    , [binary "++" ListAdd]
    , [binary "+" Add, binary "-" Sub]
    , --    , [binary ">>" Then]
      [binary "|>" Pipeline]
    , [binary ">>=" bindExpr]
    , [binary ">>" sequenceExpr]
    , [binary "==" Eq, binary "!=" Neq, binary "<=" Le, binary ">=" Ge, binary "<" Lt, binary ">" Gt]
    , [binary "&&" And, binary "||" Or]
    -- , [prefixAny pFunctionCall]
    -- , [postfixAny pFunctionCall]
    ]

sequenceExpr :: Expr -> Expr -> Expr
sequenceExpr a b = FuncCall "sequence" [a, b] anyPosition

bindExpr :: Expr -> Expr -> Expr
bindExpr a b = FuncCall "bind" [a, b] anyPosition

binaryFunctionCall :: String -> Expr -> Expr -> Expr
binaryFunctionCall f a b = FuncCall f [a, b] anyPosition

pFunctionCall :: String -> Expr -> Expr
pFunctionCall f a = FuncCall f [a] anyPosition

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

binaryAny :: (String -> Expr -> Expr -> Expr) -> Operator Parser Expr
binaryAny f = InfixL (f <$> freeOperator)

binaryR :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryR name f = InfixR (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

prefixAny :: (String -> Expr -> Expr) -> Operator Parser Expr
prefixAny f = Prefix (f <$> identifier)

postfixAny :: (String -> Expr -> Expr) -> Operator Parser Expr
postfixAny f = Postfix (f <$> identifier)

ref :: Parser Expr
ref = do
    symbol "*"
    Ref <$> funcCall

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

curlyBrackets :: Parser a -> Parser a
curlyBrackets = between (symbol "{") (symbol "}")

squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol "[") (symbol "]")

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
    p = some (alphaNumChar <|> char '_' <|> oneOf ['+', '-', '*', '/', '=', '&', '|', '!', '?', '$', '%', '^', '~'])
    check x =
        if x `elem` rws
            then fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else return x

integer :: Parser Integer
integer =
    lexeme
        ( try (char '0' *> char 'x' *> L.hexadecimal)
            <|> try L.decimal
        )

float :: Parser Float
float = lexeme (L.float <* optional (char 'f'))

double :: Parser Double
double = lexeme (L.float <* char 'd')

freeOperator :: Parser String
freeOperator = try $ lexeme $ some (oneOf ['+', '-', '*', '/', '=', '&', '|', '!', '?', '$', '%', '^', '~', ':']) >>= \x -> if length x > 1 then return x else fail "Operator must be at least two characters"

extra :: Parser String
extra = do
    name <- (lexeme . try) (p >>= check)
    if name `elem` rws
        then fail $ "keyword " ++ show name ++ " cannot be an identifier"
        else return name
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_' <|> char '@')
    check x =
        if x `elem` rws
            then fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else return x

expr :: Parser Expr
expr = makeExprParser term binOpTable

validType :: Parser Type
validType =
    do
        do
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
            squareBrackets $ do
                List <$> validType
        <|> do
            keyword "Self"
            return Self
        <|> do
            lookAhead $ satisfy isUpper
            StructT <$> identifier
        <?> "type"

stringLit :: Parser String
stringLit = lexeme $ char '\"' *> manyTill L.charLiteral (char '\"')

charLit :: Parser Char
charLit = lexeme (char '\'' *> L.charLiteral <* char '\'') <|> lexeme (L.decimal <* char 'c' >>= \x -> if x > 255 then fail "Char literal must be between 0 and 255" else return (chr x))

funcDec :: Parser Expr
funcDec = do
    name <- (identifier <|> gravis) <?> "function name"
    generics <- fromMaybe [] <$> optional generic <?> "function generics"
    symbol "::"
    argTypes <- sepBy1 validType (symbol "->") <?> "function arguments"
    return $ FuncDec name argTypes generics

defArg :: Parser Expr
defArg = try structLit <|> var <|> parens listPattern <|> array <|> placeholder <|> IntLit <$> integer <|> StringLit <$> stringLit

funcDef :: Parser Expr
funcDef = do
    name <- identifier <|> gravis <?> "function name"
    args <- many defArg <?> "function arguments"
    symbol "="
    FuncDef name args <$> expr <?> "function body"

gravis :: Parser String
gravis = lexeme $ char '`' *> someTill L.charLiteral (char '`')

funcCall :: Parser Expr
funcCall = do
    start <- getOffset
    name <- (extra <|> gravis) <?> "function name"
    args <- sepBy1 expr (symbol ",") <?> "function arguments"
    end <- getOffset
    return $ FuncCall name args (Position (start, end))

parenApply :: Parser Expr
parenApply = do
    start <- getOffset
    paren <- parens expr <?> "parenthesized function expression"
    args <- sepBy1 expr (symbol ",") <?> "function arguments"
    end <- getOffset
    return $ ParenApply paren args (Position (start, end))

letExpr :: Parser Expr
letExpr = do
    keyword "let"
    name <- identifier <|> gravis <?> "variable name"
    symbol "="
    value <- recover expr <?> "variable value"
    state <- get
    put state{validLets = name : validLets state}
    return $ Let name value

ifExpr :: Parser Expr
ifExpr = do
    keyword "if"
    cond <- expr <?> "condition"
    keyword "then"
    optional newline'
    thenExpr <- expr <?> "then expression"
    optional newline'
    keyword "else"
    optional newline'
    elseExpr <- expr <?> "else expression"
    optional newline'
    return $ If cond thenExpr elseExpr

doBlock :: Parser Expr
doBlock = do
    keyword "do"
    newline'
    exprs <- lines' <?> "do block"
    keyword "end" <|> lookAhead (keyword "else")
    return $ DoBlock exprs

generic :: Parser [GenericExpr]
generic = do
    symbol "<"
    args <- sepBy1 typeAndMaybeConstraint (symbol ",") <?> "generic arguments"
    symbol ">"
    return args
  where
    typeAndMaybeConstraint = do
        name <- identifier
        constraint <- optional $ do
            symbol ":"
            validType
        return $ GenericExpr name constraint

combinedFunc :: Parser Expr
combinedFunc = do
    keyword "let"
    name <- identifier <|> gravis <?> "function name"
    generics <- (fromMaybe [] <$> optional generic) <?> "function generics"
    (args, argTypes) <- (parens argsAndTypes <|> argsAndTypes) <?> "function arguments"
    returnType <- optional (symbol "=>" >> validType <?> "return type") <?> "return type"
    symbol "="
    body <- recover expr <?> "function body"
    return $ Function [FuncDef name args body] (FuncDec name (argTypes ++ [fromMaybe Any returnType]) generics)
  where
    argsAndTypes =
        unzip <$> many do
            arg <- defArg <?> "function argument"
            symbol ":"
            argType <- validType <?> "function argument type"
            optional $ symbol "->"
            return (arg, argType) <?> "function arguments"

import_ :: Parser Expr
import_ = do
    keyword "import"
    qualified <- optional (keyword "qualified" >> return True) <?> "qualified"
    optional $ keyword "from"
    from <- many (alphaNumChar <|> char '.' <|> char '@' <|> char '/') <?> "import path"
    alias <- optional (keyword " as" >> extra) <?> "import alias"
    return $ Import{objects = ["*"], from = from, qualified = fromMaybe False qualified, as = alias}

array :: Parser Expr
array = do
    symbol "["
    elements <- sepBy expr (symbol ",") <?> "array elements"
    symbol "]"
    return $ ListLit elements

struct :: Parser Expr
struct = do
    keyword "struct"
    name <- extra <?> "struct name"
    symbol "="
    fields <- parens (structField `sepBy` symbol ",") <?> "struct fields"
    refinementSrc <- lookAhead $ optional $ do
        keyword "satisfies"
        parens (many (noneOf [')'])) <?> "refinement source"
    refinement <- optional $ do
        keyword "satisfies"
        parens expr <?> "refinement"
    is <- optional $ do
        keyword "is"
        sepBy extra (symbol ",") <?> "struct interfaces"
    return $ Struct{name = name, fields = fields, refinement = refinement, refinementSrc = fromMaybe "" refinementSrc, is = fromMaybe [] is}
  where
    structField = do
        fieldName <- identifier <?> "field name"
        symbol ":"
        fieldType <- validType <?> "field type"
        return (fieldName, fieldType)

structLit :: Parser Expr
structLit = do
    start <- getOffset
    name <- extra <?> "struct name"
    symbol "{"
    fields <-
        sepBy
            ( do
                fieldName <- identifier <?> "field name"
                symbol ":"
                fieldValue <- expr <?> "field value"
                return (fieldName, fieldValue)
            )
            (symbol ",")
    symbol "}"
    end <- getOffset
    return $ StructLit name fields (Position (start, end))

arrayAccess :: Parser Expr
arrayAccess = do
    a <- choice [var, array] <?> "array"
    symbol "["
    index <- expr <?> "array index"
    symbol "]"
    return $ ArrayAccess a index

placeholder :: Parser Expr
placeholder = symbol "_" >> return Placeholder

parseProgramPure :: Text -> Program
parseProgramPure t = case parseProgram t initCompilerFlags of
    Left err -> error $ show err
    Right program' -> program'

parseProgram' :: Text -> State ParserState (Either (ParseErrorBundle Text Void) Program)
parseProgram' = runParserT program ""

parseProgram :: Text -> CompilerFlags -> Either (ParseErrorBundle Text Void) Program
parseProgram t cf = do
    let (result, _) = runState (parseProgram' t) (ParserState{compilerFlags = cf, validLets = [], validFunctions = []})
    case result of
        Left err -> Left err
        Right program' -> do
            -- If theres no main function, add one
            let foundMain = any (\case FuncDef "main" _ _ -> True; _ -> False) program'.exprs || any (\case Function defs _ -> any (\case FuncDef "main" _ _ -> True; _ -> False) defs; _ -> False) program'.exprs
            let (outside, inside) = partition shouldBeOutside program'.exprs
            let artificialMainProgram = Program $ outside ++ [FuncDec "main" [Any] [], FuncDef "main" [] (DoBlock inside)]
            if foundMain || not cf.needsMain
                then Right program'
                else Right artificialMainProgram
  where
    shouldBeOutside :: Expr -> Bool
    shouldBeOutside Trait{} = True
    shouldBeOutside Impl{} = True
    shouldBeOutside External{} = True
    shouldBeOutside Import{} = True
    shouldBeOutside _ = False

parseFreeUnsafe :: Text -> Expr
parseFreeUnsafe t = case parseProgram t initCompilerFlags{needsMain = False} of
    Left err -> error $ show err
    Right program' -> case program' of
        Program [expr'] -> replacePositionWithAnyPosition expr'
        _ -> error "Expected a single expression"
  where
    replacePositionWithAnyPosition :: Expr -> Expr
    replacePositionWithAnyPosition (FuncCall a b _) = FuncCall a (map replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Var a _) = Var a anyPosition
    replacePositionWithAnyPosition fd@FuncDef{body} = fd{body = replacePositionWithAnyPosition body}
    replacePositionWithAnyPosition (DoBlock a) = DoBlock $ map replacePositionWithAnyPosition a
    replacePositionWithAnyPosition (ListConcat a b) = ListConcat (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b)
    replacePositionWithAnyPosition (StructAccess a b) = StructAccess (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b)
    replacePositionWithAnyPosition x = x

lines' :: Parser [Expr]
lines' = expr `sepEndBy` newline'

program :: Parser Program
program = Program <$> between scn eof lines'

var :: Parser Expr
var = do
    start' <- getOffset
    name <- extra <|> gravis
    Var name . position start' <$> getOffset

target :: Parser Expr
target = do
    symbol "__target"
    target' <- (symbol "wasm" <|> symbol "c") <?> "target"
    Target (unpack target') <$> expr <?> "target"

listPattern :: Parser Expr
listPattern = do
    elements <- sepBy1 (var <|> placeholder <|> array) (symbol ":") <?> "list pattern"
    return $ ListPattern elements

lambda :: Parser Expr
lambda = do
    symbol "\\"
    args <- some (var <|> parens listPattern <|> array) <?> "lambda arguments"
    symbol "->"
    Lambda args <$> expr <?> "lambda body"

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
typeLiteral = do
    TypeLit <$> validType

trait :: Parser Expr
trait = do
    keyword "trait"
    name <- identifier <?> "trait name"
    methods <-
        ( do
                symbol "="
                keyword "do"
                newline'
                fds <- funcDec `sepEndBy` newline' <?> "trait methods"
                keyword "end"
                return fds
            )
            <|> return []
    return $ Trait name methods

impl :: Parser Expr
impl = do
    keyword "impl"
    name <- identifier <?> "impl name"
    symbol "for"
    for <- identifier <?> "impl for"
    methods <-
        ( do
                symbol "="
                symbol "do"
                newline'
                fds <- funcDef `sepEndBy` newline' <?> "impl methods"
                symbol "end"
                return fds
            )
            <|> return []
    return $ Impl name for methods

external :: Parser Expr
external = do
    keyword "external"
    from <- stringLit <?> "external from"
    symbol "="
    symbol "do"
    newline'
    decs <- funcDec `sepEndBy` newline' <?> "external declarations"
    symbol "end"
    return $ External from decs

unaryMinus :: Parser Expr
unaryMinus = parens $ do
    symbol "-"
    UnaryMinus <$> expr <?> "negatable expression"

term :: Parser Expr
term =
    choice
        [ placeholder
        , try unaryMinus
        , try parenApply
        , parens expr
        , CharLit <$> try charLit
        , DoubleLit <$> try double
        , FloatLit <$> try float
        , IntLit <$> try integer
        , StringLit <$> try stringLit
        , symbol "True" >> return (BoolLit True)
        , symbol "False" >> return (BoolLit False)
        , external
        , struct
        , import_
        , impl
        , Parser.trait
        , doBlock
        , try combinedFunc
        , try funcDef
        , try funcDec
        , try lambda
        , try structLit
        , array
        , try funcCall
        , try arrayAccess
        , ifExpr
        , var
        ]
