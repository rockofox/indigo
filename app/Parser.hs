module Parser where

import Data.List (intercalate)
import Debug.Trace
import Text.Parsec
import Text.Parsec.Expr (Assoc (AssocLeft, AssocNone, AssocRight), Operator (Infix), buildExpressionParser)
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
      Token.reservedNames = ["func", "let", "in", "if", "then", "else", "True", "False", "do"],
      Token.reservedOpNames = ["+", "-", "*", "/", "=", "==", "<", ">", "<=", ">=", "&&", "||"]
    }

data Expr
  = Var String
  | BoolLit Bool
  | IntLit Integer
  | BinOp String Expr Expr
  | If Expr Expr Expr
  | Let String Expr Expr
  | FuncDef String [String] Expr
  | FuncCall String [Expr]
  | DoBlock [Expr]
  deriving
    ( Show
    )

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef

identifier :: Parser String
identifier = Token.identifier lexer

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

funcDef :: Parser Expr
funcDef = do
  reserved "func"
  name <- identifier
  args <- many identifier
  reservedOp "="
  FuncDef name args <$> expr

funcCall :: Parser Expr
funcCall = do
  name <- identifier
  -- reservedOp "("
  args <- many expr
  -- reservedOp ")"
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
  DoBlock <$> sepEndBy1 expr (reservedOp ";")

parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr ""

newtype Program = Program [Expr] deriving (Show)

parseProgram :: String -> Either ParseError Program
parseProgram = parse (Program <$> sepEndBy1 expr (reservedOp ";")) ""

-- table =
--   [ [ Infix (reservedOp "*" >> return (BinOp "*")) AssocLeft,
--       Infix (reservedOp "/" >> return (BinOp "/")) AssocLeft
--     ],
--     [ Infix (reservedOp "+" >> return (BinOp "+")) AssocLeft,
--       Infix (reservedOp "-" >> return (BinOp "-")) AssocLeft
--     ],
--     [ Infix (reservedOp "==" >> return (BinOp "==")) AssocNone,
--       Infix (reservedOp "<" >> return (BinOp "<")) AssocNone,
--       Infix (reservedOp ">" >> return (BinOp ">")) AssocNone,
--       Infix (reservedOp "<=" >> return (BinOp "<=")) AssocNone,
--       Infix (reservedOp ">=" >> return (BinOp ">=")) AssocNone
--     ],
--     [ Infix (reservedOp "&&" >> return (BinOp "&&")) AssocRight,
--       Infix (reservedOp "||" >> return (BinOp "||")) AssocRight
--     ]
--   ]

var :: Parser Expr
var = do
  name <- identifier
  notFollowedBy (reservedOp "(")
  return $ Var name

term =
  parens expr
    -- <|> fmap Var identifier
    -- <|> try var
    <|> funcDef
    <|> doBlock
    <|> funcCall
    <|> (reserved "True" >> return (BoolLit True))
    <|> (reserved "False" >> return (BoolLit False))
    <|> fmap IntLit integer
    <|> letExpr
    <|> ifExpr