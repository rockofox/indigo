module Parser where

import Debug.Trace (trace, traceShow)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token qualified as Token

languageDef :: Token.LanguageDef ()
languageDef =
  emptyDef
    { Token.commentStart = "{-",
      Token.commentEnd = "-}",
      Token.commentLine = "--",
      Token.identStart = letter <|> char '_',
      Token.identLetter = alphaNum <|> char '_',
      Token.opStart = Token.opLetter languageDef,
      Token.opLetter = oneOf "+-*/=",
      Token.reservedNames = ["func", "let", "in", "if", "then", "else", "do", "end", "is", "extern"],
      Token.reservedOpNames = ["+", "-", "*", "/", "=", "==", "<", ">", "<=", ">=", "&&", "||", "::", "->"]
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

expr :: Parser Expr
-- expr = buildExpressionParser table term <?> "expression"
expr = term

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
funcCall =
  trace "Parsing function call" $ do
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
  notFollowedBy (reservedOp "(")
  return $ Var name

term :: Parser Expr
term =
  parens expr
    -- <|> fmap Var identifier
    -- <|> try var
    <|> stringLit
    <|> (reserved "True" >> return (BoolLit True))
    <|> (reserved "False" >> return (BoolLit False))
    <|> externDec
    <|> try funcDec
    <|> try funcDef
    <|> doBlock
    <|> funcCall
    <|> fmap IntLit integer
    <|> letExpr
    <|> ifExpr
