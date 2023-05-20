{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser where

import Control.Monad (void)
import Control.Monad.Combinators
  ( between,
    choice,
    many,
    manyTill,
    sepBy,
    sepBy1,
    sepEndBy,
    some,
    (<|>),
  )
import Control.Monad.Combinators.Expr (Operator (InfixL, Postfix, Prefix), makeExprParser)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, takeWhile1P, try),
    ParseErrorBundle,
    Parsec,
    optional,
    parse,
    (<?>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    newline,
    space1,
  )
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

data Expr
  = Var String
  | BoolLit Bool
  | IntLit Integer
  | StringLit String
  | BinOp String Expr Expr
  | If Expr Expr Expr
  | Let String Expr Expr
  | FuncDef String [String] Expr
  | FuncCall String [Expr]
  | FuncDec {fname :: String, types :: [String]}
  | ModernFunc Expr Expr
  | DoBlock [Expr]
  | ExternDec String String [String]
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
  deriving
    ( Show
    )

binOpTable :: [[Operator Parser Expr]]
binOpTable =
  [ [prefix "!" Not],
    [binary "+" Add, binary "-" Sub],
    [binary "*" Mul, binary "/" Div],
    [binary "==" Eq, binary "<" Lt, binary ">" Gt, binary "<=" Le, binary ">=" Ge],
    [binary "&&" And, binary "||" Or]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

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
rws = ["if", "then", "else", "while", "do", "end", "true", "false", "not", "and", "or"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

integer :: Parser Integer
integer = lexeme L.decimal

foreignIdentifier :: Parser String
foreignIdentifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '.')
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

expr :: Parser Expr
expr = makeExprParser term binOpTable

validType :: Parser String
validType =
  do
    symbol "Int"
    return "Int"
    <|> do
      symbol "Bool"
      return "Bool"
    <|> do
      symbol "String"
      return "String"
    <|> do
      symbol "Nothing"
      return "Nothing"
    <|> do
      symbol "Any"
      return "Any"
    <?> "type"

stringLit :: Parser String
stringLit = char '\"' *> manyTill L.charLiteral (char '\"')

-- TODO: take a look at this in context of modern function syntax and consider making them consistent
externDec :: Parser Expr
externDec = do
  symbol "extern"
  lang <- identifier
  name <- foreignIdentifier
  symbol "::"
  args <- sepBy validType (symbol "->")
  return $ ExternDec lang name args

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
  name <- foreignIdentifier
  args <- some expr
  return $ FuncCall name args

letExpr :: Parser Expr
letExpr = do
  symbol "let"
  name <- identifier
  symbol "="
  val <- expr
  symbol "in"
  Let name val <$> expr

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
  body <- expr
  return $ ModernFunc (FuncDef name args body) (FuncDec name (returnType : argTypes))

placeholder :: Parser Expr
placeholder = symbol "()" >> return Placeholder

newtype Program = Program [Expr] deriving (Show)

parseProgram :: Text -> Either (ParseErrorBundle Text Void) Program
parseProgram = parse program ""

lines' :: Parser [Expr]
lines' = expr `sepEndBy` newline'

program :: Parser Program
program = Program <$> between scn eof lines'

var :: Parser Expr
var = do
  Var <$> identifier

term :: Parser Expr
term =
  choice
    [ placeholder,
      parens expr,
      fmap IntLit integer,
      fmap StringLit stringLit,
      symbol "True" >> return (BoolLit True),
      symbol "False" >> return (BoolLit False),
      try externDec,
      doBlock,
      try modernFunction,
      try funcDec,
      try funcDef,
      try funcCall,
      letExpr,
      ifExpr,
      var
    ]