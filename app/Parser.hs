{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Parser where

import Data.Functor.Identity qualified
import Text.Parsec
import Text.Parsec.Expr (Assoc (AssocLeft), Operator (Infix, Prefix, Postfix), buildExpressionParser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as Token

languageDef :: Token.LanguageDef ()
languageDef =
  emptyDef
    { Token.commentStart = "{-"
    , Token.commentEnd = "-}"
    , Token.commentLine = "--"
    , Token.identStart = letter <|> char '_'
    , Token.identLetter = alphaNum <|> char '_'
    , Token.opStart = Token.opLetter languageDef
    , Token.opLetter = oneOf "+-*/="
    , Token.reservedNames = ["func", "let", "in", "if", "then", "else", "do", "end", "is", "extern"]
    , Token.reservedOpNames = ["+", "-", "*", "/", "=", "==", "<", ">", "<=", ">=", "&&", "||", "::", "->"]
    }

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
  | DoBlock [Expr]
  | ExternDec String String [String] -- extern javascript console.log s;
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
  deriving
    ( Show
    )

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer

lexeme :: Parser a -> Parser a
lexeme = Token.lexeme lexer

integer :: Parser Integer
integer = Token.integer lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

binOpTable :: [[Operator String () Data.Functor.Identity.Identity Expr]]
binOpTable =
  [ [binary "+" Add AssocLeft, binary "-" Sub AssocLeft]
  , [binary "*" Mul AssocLeft, binary "/" Div AssocLeft]
  , [binary "==" Eq AssocLeft, binary "<" Lt AssocLeft, binary ">" Gt AssocLeft, binary "<=" Le AssocLeft, binary ">=" Ge AssocLeft]
  , [binary "&&" And AssocLeft, binary "||" Or AssocLeft]
  , [prefix "!" Not]
  ]

binary name fun = Infix (do reservedOp name; return fun)
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })

expr :: Parser Expr
expr = buildExpressionParser binOpTable term <?> "expression"

validType :: Parser String
validType =
  do
    reserved "Int"
    return "Int"
    <|> do
      reserved "Bool"
      return "Bool"
    <|> do
      reserved "String"
      return "String"
    <|> do
      reserved "Nothing"
      return "Nothing"
    <|> do
      reserved "Any"
      return "Any"
    <?> "type"

stringLit :: Parser Expr
stringLit = do
  _ <- char '"'
  str <- many (noneOf "\"")
  _ <- char '"'
  return $ StringLit str

externDec :: Parser Expr
externDec = do
  reserved "extern"
  lang <- identifier
  name <- many1 (noneOf " \t") <* reservedOp " "
  reservedOp "::"
  args <- sepBy validType (reservedOp "->")
  return $ ExternDec lang name args

funcDec :: Parser Expr
funcDec = do
  name <- identifier
  reservedOp "::"
  types <- sepBy1 validType (reservedOp "->")
  return $ FuncDec name types

funcDef :: Parser Expr
funcDef = do
  name <- identifier <?> "function name"
  args <- many identifier <?> "function formal parameters"
  reservedOp "="
  FuncDef name args <$> expr

funcCall :: Parser Expr
funcCall = do
  name <- lexeme $ try $ do
    name <- many1 (alphaNum <|> char '.' <|> char '_')
    reservedOp " "
    notFollowedBy (oneOf "=") -- TODO: Refactor out, consider all reserved words
    return name
  args <- many expr <?> "function arguments"
  return $ FuncCall name args

letExpr :: Parser Expr
letExpr = do
  reserved "let"
  name <- identifier
  reservedOp "="
  val <- expr
  reserved "in"
  Let name val <$> expr

ifExpr :: Parser Expr
ifExpr = do
  reserved "if"
  cond <- expr
  reserved "then"
  thenExpr <- expr
  reserved "else"
  If cond thenExpr <$> expr

doBlock :: Parser Expr
doBlock = do
  reserved "do"
  DoBlock <$> sepEndBy1 expr (reservedOp ";") <* reserved "end"

parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr ""

newtype Program = Program [Expr] deriving (Show)

parseProgram :: String -> Either ParseError Program
parseProgram = parse (Program <$> sepEndBy1 expr (reservedOp ";")) ""

var :: Parser Expr
var = do
  name <- identifier
  notFollowedBy (oneOf "=() ") -- TODO: Refactor out, consider all reserved words
  return $ Var name

term :: Parser Expr
term =
  parens expr
    -- <|> fmap Var identifier
    <|> stringLit
    <|> (reserved "True" >> return (BoolLit True))
    <|> (reserved "False" >> return (BoolLit False))
    <|> externDec
    <|> try funcDec
    <|> try funcDef
    <|> doBlock
    <|> fmap IntLit integer
    <|> funcCall
    <|> letExpr
    <|> ifExpr
    <|> var
