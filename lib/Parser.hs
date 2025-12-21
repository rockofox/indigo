{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parser (Expr (..), Program (..), parseProgram', parseProgramPure, ParserState (..), Type (..), parseProgram, parseFreeUnsafe, CompilerFlags (..), initCompilerFlags, typeOf, compareTypes) where

import AST
import Control.Applicative.Combinators (someTill)
import Control.Monad (when)
import Control.Monad qualified
import Control.Monad.Combinators
  ( between,
    choice,
    many,
    manyTill,
    sepBy,
    sepBy1,
    sepEndBy,
    (<|>),
  )
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.State
  ( MonadState (get, put),
    State,
    runState,
  )
import Data.Bifunctor (bimap, second)
import Data.Char (isUpper)
import Data.List
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Void (Void)
import GHC.Char (chr)
import Text.Megaparsec
  ( MonadParsec (eof, lookAhead, notFollowedBy, takeWhile1P, try, withRecovery),
    ParseError (..),
    ParseErrorBundle (..),
    ParsecT,
    getOffset,
    noneOf,
    oneOf,
    optional,
    registerParseError,
    runParserT,
    satisfy,
    some,
    (<?>),
  )
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    letterChar,
    newline,
    space1,
  )
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug (dbg)

data CompilerFlags = CompilerFlags
  { verboseMode :: Bool,
    needsMain :: Bool
  }
  deriving
    ( Show
    )

initCompilerFlags :: CompilerFlags
initCompilerFlags = CompilerFlags {verboseMode = False, needsMain = True}

data ParserState = ParserState
  { validFunctions :: [String],
    validLets :: [String],
    compilerFlags :: CompilerFlags
  }
  deriving
    ( Show
    )

type Parser = ParsecT Void Text (State ParserState)

binOpTable :: [[Operator Parser Expr]]
binOpTable =
  [ [prefix "^" Flexible],
    [binary "." tupleOrStructAccess],
    [binary "as" Cast],
    [prefix "$" StrictEval],
    [prefix "!" Not],
    [binary "++" ListAdd],
    [binary "**" Power, binary "*" Mul, binary "/" Div],
    [binary "+" Add, binary "-" Sub],
    [binaryAny (\op a b _p -> binaryFunctionCall op a b)],
    -- , [prefix "-" UnaryMinus]
    [binary "%" Modulo],
    [binary ":" ListConcat],
    --    , [binary ">>" Then]
    [binary "|>" Pipeline],
    [binary ">>=" bindExpr],
    [binary ">>" sequenceExpr],
    [binary "==" Eq, binary "!=" Neq, binary "<=" Le, binary ">=" Ge, binary "<" Lt, binary ">" Gt],
    [binary "&&" And, binary "||" Or]
    -- , [prefixAny pFunctionCall]
    -- , [postfixAny pFunctionCall]
  ]

sequenceExpr :: Expr -> Expr -> Position -> Expr
sequenceExpr a b p = FuncCall {funcName = "sequence", funcArgs = [a, b], funcPos = p}

bindExpr :: Expr -> Expr -> Position -> Expr
bindExpr a b p = FuncCall {funcName = "bind", funcArgs = [a, b], funcPos = p}

binaryFunctionCall :: String -> Expr -> Expr -> Expr
binaryFunctionCall f a b = FuncCall {funcName = f, funcArgs = [a, b], funcPos = anyPosition}

pFunctionCall :: String -> Expr -> Expr
pFunctionCall f a = FuncCall {funcName = f, funcArgs = [a], funcPos = anyPosition}

getExprPosition :: Expr -> Position
getExprPosition (Var {varPos}) = varPos
getExprPosition (FuncCall {funcPos}) = funcPos
getExprPosition (StructLit {structLitPos}) = structLitPos
getExprPosition (ParenApply {parenApplyPos}) = parenApplyPos
getExprPosition (BoolLit {boolPos}) = boolPos
getExprPosition (IntLit {intPos}) = intPos
getExprPosition (StringLit {stringPos}) = stringPos
getExprPosition (FloatLit {floatPos}) = floatPos
getExprPosition (DoubleLit {doublePos}) = doublePos
getExprPosition (CharLit {charPos}) = charPos
getExprPosition (If {ifPos}) = ifPos
getExprPosition (Let {letPos}) = letPos
getExprPosition (FuncDef {funcDefPos}) = funcDefPos
getExprPosition (FuncDec {funcDecPos}) = funcDecPos
getExprPosition (Function {functionPos}) = functionPos
getExprPosition (DoBlock {doBlockPos}) = doBlockPos
getExprPosition (ExternDec {externDecPos}) = externDecPos
getExprPosition (Add {addPos}) = addPos
getExprPosition (Sub {subPos}) = subPos
getExprPosition (Mul {mulPos}) = mulPos
getExprPosition (Div {divPos}) = divPos
getExprPosition (Eq {eqPos}) = eqPos
getExprPosition (Neq {neqPos}) = neqPos
getExprPosition (Lt {ltPos}) = ltPos
getExprPosition (Gt {gtPos}) = gtPos
getExprPosition (Le {lePos}) = lePos
getExprPosition (Ge {gePos}) = gePos
getExprPosition (And {andPos}) = andPos
getExprPosition (Or {orPos}) = orPos
getExprPosition (Not {notPos}) = notPos
getExprPosition (UnaryMinus {unaryMinusPos}) = unaryMinusPos
getExprPosition (Placeholder {placeholderPos}) = placeholderPos
getExprPosition (Discard {discardPos}) = discardPos
getExprPosition (Import {importPos}) = importPos
getExprPosition (Ref {refPos}) = refPos
getExprPosition (Struct {structPos}) = structPos
getExprPosition (StructAccess {structAccessPos}) = structAccessPos
getExprPosition (ListLit {listLitPos}) = listLitPos
getExprPosition (ListPattern {listPatternPos}) = listPatternPos
getExprPosition (ListConcat {listConcatPos}) = listConcatPos
getExprPosition (ListAdd {listAddPos}) = listAddPos
getExprPosition (ArrayAccess {arrayAccessPos}) = arrayAccessPos
getExprPosition (Modulo {moduloPos}) = moduloPos
getExprPosition (Power {powerPos}) = powerPos
getExprPosition (Target {targetPos}) = targetPos
getExprPosition (Then {thenPos}) = thenPos
getExprPosition (Pipeline {pipelinePos}) = pipelinePos
getExprPosition (Lambda {lambdaPos}) = lambdaPos
getExprPosition (Cast {castPos}) = castPos
getExprPosition (TypeLit {typeLitPos}) = typeLitPos
getExprPosition (Flexible {flexiblePos}) = flexiblePos
getExprPosition (Trait {traitPos}) = traitPos
getExprPosition (Impl {implPos}) = implPos
getExprPosition (StrictEval {strictEvalPos}) = strictEvalPos
getExprPosition (External {externalPos}) = externalPos
getExprPosition (When {whenPos}) = whenPos
getExprPosition (TupleLit {tupleLitPos}) = tupleLitPos
getExprPosition (TupleAccess {tupleAccessPos}) = tupleAccessPos

combinePositions :: Position -> Position -> Position
combinePositions (Position (x1, x2)) (Position (y1, y2))
  | x1 >= 0 && y1 >= 0 = Position (min x1 y1, max x2 y2)
  | x1 >= 0 = Position (x1, x2)
  | y1 >= 0 = Position (y1, y2)
  | otherwise = anyPosition

binary :: Text -> (Expr -> Expr -> Position -> Expr) -> Operator Parser Expr
binary name f = InfixL $ do
  _ <- symbol name
  return $ \a b -> f a b (combinePositions (getExprPosition a) (getExprPosition b))

tupleOrStructAccess :: Expr -> Expr -> Position -> Expr
tupleOrStructAccess a b pos = case b of
  IntLit {intValue} -> TupleAccess a (fromIntegral intValue) pos
  FloatLit {floatValue}
    | fromInteger (round floatValue) == floatValue ->
        TupleAccess a (round floatValue) pos
  _ -> StructAccess a b pos

binaryAny :: (String -> Expr -> Expr -> Position -> Expr) -> Operator Parser Expr
binaryAny f = InfixL $ do
  op <- freeOperator
  return $ \a b -> f op a b (combinePositions (getExprPosition a) (getExprPosition b))

binaryR :: Text -> (Expr -> Expr -> Position -> Expr) -> Operator Parser Expr
binaryR name f = InfixR $ do
  _ <- symbol name
  return $ \a b -> f a b (combinePositions (getExprPosition a) (getExprPosition b))

prefix, postfix :: Text -> (Expr -> Position -> Expr) -> Operator Parser Expr
prefix name f = Prefix $ do
  start <- getOffset
  _ <- symbol name
  end <- getOffset
  let pos = Position (start, end)
  return $ \a -> f a (combinePositions pos (getExprPosition a))
postfix name f = Postfix $ do
  start <- getOffset
  _ <- symbol name
  end <- getOffset
  let pos = Position (start, end)
  return $ \a -> f a (combinePositions (getExprPosition a) pos)

prefixAny :: (String -> Expr -> Expr) -> Operator Parser Expr
prefixAny f = Prefix (f <$> identifier)

postfixAny :: (String -> Expr -> Expr) -> Operator Parser Expr
postfixAny f = Postfix (f <$> identifier)

ref :: Parser Expr
ref = do
  start <- getOffset
  symbol "*"
  expr' <- funcCall
  end <- getOffset
  return $ Ref expr' (Position (start, end))

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
rws = ["if", "then", "else", "do", "end", "True", "False", "let", "as", "when", "of", "it", "satisfies", "is", "module", "import", "qualified", "type"]

identifier :: Parser String
identifier = do
  name <- (lexeme . try) (p >>= check)
  if name `elem` rws
    then fail $ "keyword " ++ show name ++ " cannot be an identifier"
    else return name
  where
    p = some (alphaNumChar <|> char '_' <|> oneOf ['+', '-', '*', '/', '=', '&', '|', '!', '?', '%', '^', '~'])
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
freeOperator = try $ lexeme $ do
  x <- some (oneOf ['+', '-', '*', '/', '=', '&', '|', '!', '?', '%', '^', '~', ':'])
  let predefinedOps = ["!=", "==", "<=", ">=", "&&", "||"]
  if length x > 1
    then
      if x `elem` predefinedOps
        then fail $ "Overriding predefined operator " ++ show x ++ " is not allowed"
        else return x
    else fail "Operator must be at least two characters"

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

qualifiedIdentifier :: Parser String
qualifiedIdentifier = do
  name <- (lexeme . try) (p >>= check)
  if name `elem` rws
    then fail $ "keyword " ++ show name ++ " cannot be an identifier"
    else return name
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_' <|> char '@' <|> char '.')
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

expr :: Parser Expr
expr = makeExprParser term binOpTable

validType :: Parser Type
validType =
  ( do
      keyword "Any"
      return Any
  )
    <|> try
      ( do
          parens $ do
            firstType <- validType
            hasCommaAfterFirst <- optional (symbol ",")
            case hasCommaAfterFirst of
              Just _ -> do
                nextIsClose <- lookAhead (optional (symbol ")"))
                case nextIsClose of
                  Just _ -> return $ Tuple [firstType]
                  Nothing -> do
                    restTypes <- sepBy validType (symbol ",")
                    optional (symbol ",")
                    return $ Tuple (firstType : restTypes)
              Nothing -> fail "Tuple type must have at least 2 elements or a trailing comma for single-element tuple"
      )
    <|> try
      ( do
          parens $ do
            startsWithArrow <- optional (symbol "->")
            case startsWithArrow of
              Just _ -> do
                Fn [] <$> validType
              Nothing -> do
                firstType <- validType
                hasArrow <- optional (symbol "->")
                case hasArrow of
                  Just _ -> do
                    restTypes <- sepBy validType (symbol "->")
                    let allTypes = firstType : restTypes
                        ret = last allTypes
                        args = init allTypes
                     in return $ Fn args ret
                  Nothing -> fail "Not a function type"
      )
    <|> ( do
            parens validType
        )
    <|> ( do
            squareBrackets $ List <$> validType
        )
    <|> ( do
            keyword "Self"
            return Self
        )
    <|> try
      ( do
          lookAhead $ satisfy isUpper
          structName <- identifier
          typeArgs <- optional $ do
            symbol "<"
            args <- sepBy1 validType (symbol ",") <?> "type arguments"
            symbol ">"
            return args
          return $ StructT structName (fromMaybe [] typeArgs)
      )
    <?> "type"

stringLit :: Parser String
stringLit = lexeme $ char '"' *> manyTill L.charLiteral (char '"')

charLit :: Parser Char
charLit = lexeme (char '\'' *> L.charLiteral <* char '\'') <|> lexeme (L.decimal <* char 'c' >>= \x -> if x > 255 then fail "Char literal must be between 0 and 255" else return (chr x))

funcDec :: Parser Expr
funcDec = do
  start <- getOffset
  name <- (identifier <|> gravis) <?> "function name"
  generics <- fromMaybe [] <$> optional generic <?> "function generics"
  symbol "::"
  argTypes <- sepBy1 validType (symbol "->") <?> "function arguments"
  end <- getOffset
  return $ FuncDec name argTypes generics (Position (start, end))

defArg :: Parser Expr
defArg =
  try structLit
    <|> try tupleLit
    <|> var
    <|> parens listPattern
    <|> array
    <|> placeholder
    <|> do
      start <- getOffset
      val <- integer
      end <- getOffset
      return $ IntLit val (Position (start, end))
    <|> do
      start <- getOffset
      val <- stringLit
      end <- getOffset
      return $ StringLit val (Position (start, end))
    <|> do
      start <- getOffset
      symbol "True"
      end <- getOffset
      return $ BoolLit True (Position (start, end))
    <|> do
      start <- getOffset
      symbol "False"
      end <- getOffset
      return $ BoolLit False (Position (start, end))

funcDef :: Parser Expr
funcDef = do
  start <- getOffset
  name <- identifier <|> gravis <?> "function name"
  args <- many defArg <?> "function arguments"
  symbol "="
  body <- expr <?> "function body"
  end <- getOffset
  return $ FuncDef name args body (Position (start, end))

gravis :: Parser String
gravis = lexeme $ char '`' *> someTill L.charLiteral (char '`')

funcCall :: Parser Expr
funcCall = do
  start <- getOffset
  name <- (qualifiedIdentifier <|> gravis) <?> "function name"
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
  start <- getOffset
  keyword "let"
  name <- identifier <|> gravis <?> "variable name"
  symbol "="
  value <- recover expr <?> "variable value"
  end <- getOffset
  state <- get
  put state {validLets = name : validLets state}
  return $ Let name value (Position (start, end))

whenExprParser :: Parser Expr
whenExprParser = do
  start <- getOffset
  keyword "when"
  whenExpr' <- expr <?> "when expression"
  keyword "of"
  newline'
  branches <- many $ do
    pat <- defArg <?> "pattern"
    symbol "->"
    body <- expr <?> "branch body"
    newline'
    return (pat, body)
  else_ <- optional $ do
    keyword "else"
    symbol "->"
    elseExpr <- expr <?> "else expression"
    newline'
    return elseExpr
  keyword "end"
  end <- getOffset
  return $ When whenExpr' branches else_ (Position (start, end))

ifExpr :: Parser Expr
ifExpr = do
  start <- getOffset
  keyword "if"
  cond <- expr <?> "condition"
  keyword "then"
  optional newline'
  thenExpr <- expr <?> "then expression"
  optional newline'
  keyword "else"
  optional newline'
  elseExpr <- expr <?> "else expression"
  end <- getOffset
  return $ If cond thenExpr elseExpr (Position (start, end))

doBlock :: Parser Expr
doBlock = do
  start <- getOffset
  keyword "do"
  newline'
  exprs <- lines' <?> "do block"
  keyword "end" <|> lookAhead (keyword "else")
  end <- getOffset
  return $ DoBlock exprs (Position (start, end))

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
  start <- getOffset
  keyword "let"
  name <- identifier <|> gravis <?> "function name"
  generics <- (fromMaybe [] <$> optional generic) <?> "function generics"
  (args, argTypes) <- (parens argsAndTypes <|> argsAndTypes) <?> "function arguments"
  returnType <- optional (symbol ":" >> validType <?> "return type") <?> "return type"
  symbol "="
  body <- recover expr <?> "function body"
  end <- getOffset
  return $ Function [FuncDef name args body anyPosition] (FuncDec name (argTypes ++ [fromMaybe Any returnType]) generics anyPosition) (Position (start, end))
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
  start <- getOffset
  keyword "import"
  qualified <- optional (keyword "qualified" >> return True) <?> "qualified"
  optional $ keyword "from"
  from <- many (alphaNumChar <|> char '.' <|> char '@' <|> char '/') <?> "import path"
  alias <- optional (keyword " as" >> extra) <?> "import alias"
  end <- getOffset
  return $ Import {objects = ["*"], from = from, qualified = Just True == qualified, as = alias, importPos = Position (start, end)}

array :: Parser Expr
array = do
  start <- getOffset
  symbol "["
  elements <- sepBy expr (symbol ",") <?> "array elements"
  symbol "]"
  end <- getOffset
  return $ ListLit elements (Position (start, end))

typeDef :: Parser Expr
typeDef = do
  start <- getOffset
  keyword "type"
  name <- extra <?> "type name"
  generics <- fromMaybe [] <$> optional generic <?> "type generics"
  symbol "="
  startsWithParen <- lookAhead (optional (symbol "("))
  case startsWithParen of
    Just _ -> do
      fields <- parens (structField `sepBy` symbol ",") <?> "type fields"
      refinementSrc <- lookAhead $ optional $ do
        keyword "satisfies"
        parens (many (noneOf [')'])) <?> "refinement source"
      refinement <- optional $ do
        keyword "satisfies"
        parens expr <?> "refinement"
      is <- optional $ do
        keyword "is"
        sepBy validType (symbol ",") <?> "type interfaces"
      end <- getOffset
      return $ Struct {name = name, fields = fields, refinement = refinement, refinementSrc = fromMaybe "" refinementSrc, is = fromMaybe [] is, isValueStruct = False, generics = generics, structPos = Position (start, end)}
    Nothing -> do
      constructors <- sepBy1 variantConstructor (symbol "|") <?> "type constructors"
      end <- getOffset
      let pos = Position (start, end)
      let traitExpr = Trait name [] [] [] Nothing "" pos
      let structExprs = map (createStruct name pos) constructors
      return $ DoBlock (traitExpr : structExprs) pos
  where
    structField = do
      fieldName <- identifier <?> "field name"
      symbol ":"
      fieldType <- validType <?> "field type"
      return (fieldName, fieldType)
    createStruct :: String -> Position -> (String, [(String, Type)]) -> Expr
    createStruct typeName pos (ctorName, ctorFields) =
      Struct
        { name = ctorName,
          fields = ctorFields,
          refinement = Nothing,
          refinementSrc = "",
          is = [StructT typeName []],
          isValueStruct = False,
          generics = [],
          structPos = pos
        }
    variantConstructor :: Parser (String, [(String, Type)])
    variantConstructor = do
      ctorName <- extra <?> "constructor name"
      ctorFields <- parens (structField `sepBy` symbol ",") <?> "constructor fields"
      return (ctorName, ctorFields)

valueType :: Parser Expr
valueType = do
  start <- getOffset
  _ <- lexeme $ try (string "value" *> notFollowedBy alphaNumChar)
  keyword "type"
  name <- extra <?> "value type name"
  generics <- fromMaybe [] <$> optional generic <?> "value type generics"
  symbol "="
  fields <- parens (structField `sepBy` symbol ",") <?> "value type field"
  when (length fields /= 1) $ fail "value type must have exactly one field"
  refinementSrc <- lookAhead $ optional $ do
    keyword "satisfies"
    parens (many (noneOf [')'])) <?> "refinement source"
  refinement <- optional $ do
    keyword "satisfies"
    parens expr <?> "refinement"
  is <- optional $ do
    keyword "is"
    sepBy validType (symbol ",") <?> "type interfaces"
  end <- getOffset
  return $ Struct {name = name, fields = fields, refinement = refinement, refinementSrc = fromMaybe "" refinementSrc, is = fromMaybe [] is, isValueStruct = True, generics = generics, structPos = Position (start, end)}
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
  typeArgs <- optional $ do
    symbol "<"
    args <- sepBy1 validType (symbol ",") <?> "type arguments"
    symbol ">"
    return args
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
  return $ StructLit name fields (fromMaybe [] typeArgs) (Position (start, end))

arrayAccess :: Parser Expr
arrayAccess = do
  start <- getOffset
  a <- choice [var, array] <?> "array"
  symbol "["
  index <- expr <?> "array index"
  symbol "]"
  end <- getOffset
  return $ ArrayAccess a index (Position (start, end))

placeholder :: Parser Expr
placeholder = do
  start <- getOffset
  symbol "_"
  end <- getOffset
  return $ Placeholder (Position (start, end))

parseProgramPure :: Text -> Program
parseProgramPure t = case parseProgram t initCompilerFlags of
  Left err -> error $ show err
  Right program' -> program'

parseProgram' :: Text -> State ParserState (Either (ParseErrorBundle Text Void) Program)
parseProgram' = runParserT program ""

parseProgram :: Text -> CompilerFlags -> Either (ParseErrorBundle Text Void) Program
parseProgram t cf = do
  let (result, _) = runState (parseProgram' t) (ParserState {compilerFlags = cf, validLets = [], validFunctions = []})
  case result of
    Left err -> Left err
    Right program' -> do
      -- If theres no main function, add one
      let foundMain = any (\case FuncDef {name = "main"} -> True; _ -> False) program'.exprs || any (\case Function {def} -> any (\case FuncDef {name = "main"} -> True; _ -> False) def; _ -> False) program'.exprs
      let (outside, inside) = partition shouldBeOutside program'.exprs
      let artificialMainProgram = Program (outside ++ [FuncDec "main" [Any] [] anyPosition, FuncDef "main" [] (DoBlock inside anyPosition) anyPosition]) Nothing
      if foundMain || not cf.needsMain
        then Right program'
        else Right artificialMainProgram
  where
    shouldBeOutside :: Expr -> Bool
    shouldBeOutside Trait {} = True
    shouldBeOutside Impl {} = True
    shouldBeOutside External {} = True
    shouldBeOutside Import {} = True
    shouldBeOutside _ = False

parseFreeUnsafe :: Text -> Expr
parseFreeUnsafe t = case parseProgram t initCompilerFlags {needsMain = False} of
  Left err -> error $ show err
  Right program' -> case program' of
    Program [expr'] _ -> replacePositionWithAnyPosition expr'
    _ -> error "Expected a single expression"
  where
    replacePositionWithAnyPosition :: Expr -> Expr
    replacePositionWithAnyPosition (FuncCall a b _) = FuncCall a (map replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Var a _) = Var a anyPosition
    replacePositionWithAnyPosition (BoolLit v _) = BoolLit v anyPosition
    replacePositionWithAnyPosition (IntLit v _) = IntLit v anyPosition
    replacePositionWithAnyPosition (StringLit v _) = StringLit v anyPosition
    replacePositionWithAnyPosition (FloatLit v _) = FloatLit v anyPosition
    replacePositionWithAnyPosition (DoubleLit v _) = DoubleLit v anyPosition
    replacePositionWithAnyPosition (CharLit v _) = CharLit v anyPosition
    replacePositionWithAnyPosition (If a b c _) = If (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) (replacePositionWithAnyPosition c) anyPosition
    replacePositionWithAnyPosition (Let n v _) = Let n (replacePositionWithAnyPosition v) anyPosition
    replacePositionWithAnyPosition fd@FuncDef {body} = fd {body = replacePositionWithAnyPosition body}
    replacePositionWithAnyPosition (FuncDec n types g _) = FuncDec n types g anyPosition
    replacePositionWithAnyPosition (Function d dec _) = Function (map replacePositionWithAnyPosition d) (replacePositionWithAnyPosition dec) anyPosition
    replacePositionWithAnyPosition (DoBlock a _) = DoBlock (map replacePositionWithAnyPosition a) anyPosition
    replacePositionWithAnyPosition (ExternDec n types a _) = ExternDec n types a anyPosition
    replacePositionWithAnyPosition (Add a b _) = Add (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Sub a b _) = Sub (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Mul a b _) = Mul (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Div a b _) = Div (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Eq a b _) = Eq (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Neq a b _) = Neq (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Lt a b _) = Lt (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Gt a b _) = Gt (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Le a b _) = Le (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Ge a b _) = Ge (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (And a b _) = And (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Or a b _) = Or (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Not a _) = Not (replacePositionWithAnyPosition a) anyPosition
    replacePositionWithAnyPosition (UnaryMinus a _) = UnaryMinus (replacePositionWithAnyPosition a) anyPosition
    replacePositionWithAnyPosition (Placeholder _) = Placeholder anyPosition
    replacePositionWithAnyPosition (Discard a _) = Discard (replacePositionWithAnyPosition a) anyPosition
    replacePositionWithAnyPosition (Import o f q a _) = Import o f q a anyPosition
    replacePositionWithAnyPosition (Ref a _) = Ref (replacePositionWithAnyPosition a) anyPosition
    replacePositionWithAnyPosition (Struct n f r s i v g _) = Struct n f (fmap replacePositionWithAnyPosition r) s i v g anyPosition
    replacePositionWithAnyPosition (StructLit n f typeArgs _) = StructLit n (map (second replacePositionWithAnyPosition) f) typeArgs anyPosition
    replacePositionWithAnyPosition (StructAccess a b _) = StructAccess (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (ListLit a _) = ListLit (map replacePositionWithAnyPosition a) anyPosition
    replacePositionWithAnyPosition (ListPattern a _) = ListPattern (map replacePositionWithAnyPosition a) anyPosition
    replacePositionWithAnyPosition (ListConcat a b _) = ListConcat (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (ListAdd a b _) = ListAdd (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (ArrayAccess a b _) = ArrayAccess (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Modulo a b _) = Modulo (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Power a b _) = Power (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Target n e _) = Target n (replacePositionWithAnyPosition e) anyPosition
    replacePositionWithAnyPosition (Then a b _) = Then (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Pipeline a b _) = Pipeline (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Lambda a b _) = Lambda (map replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (Cast a b _) = Cast (replacePositionWithAnyPosition a) (replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (TypeLit type' _) = TypeLit type' anyPosition
    replacePositionWithAnyPosition (Flexible a _) = Flexible (replacePositionWithAnyPosition a) anyPosition
    replacePositionWithAnyPosition (Trait name methods g reqProps ref refSrc _) = Trait name (map replacePositionWithAnyPosition methods) g reqProps (fmap replacePositionWithAnyPosition ref) refSrc anyPosition
    replacePositionWithAnyPosition (Impl traitName traitTypeArgs f m _) = Impl traitName traitTypeArgs f (map replacePositionWithAnyPosition m) anyPosition
    replacePositionWithAnyPosition (StrictEval a _) = StrictEval (replacePositionWithAnyPosition a) anyPosition
    replacePositionWithAnyPosition (External n a _) = External n (map replacePositionWithAnyPosition a) anyPosition
    replacePositionWithAnyPosition (ParenApply a b _) = ParenApply (replacePositionWithAnyPosition a) (map replacePositionWithAnyPosition b) anyPosition
    replacePositionWithAnyPosition (TupleLit a _) = TupleLit (map replacePositionWithAnyPosition a) anyPosition
    replacePositionWithAnyPosition (TupleAccess a i _) = TupleAccess (replacePositionWithAnyPosition a) i anyPosition
    replacePositionWithAnyPosition (When e b el _) =
      When
        (replacePositionWithAnyPosition e)
        ( map
            ( Data.Bifunctor.bimap
                replacePositionWithAnyPosition
                replacePositionWithAnyPosition
            )
            b
        )
        (fmap replacePositionWithAnyPosition el)
        anyPosition

lines' :: Parser [Expr]
lines' = expr `sepEndBy` newline'

moduleDecl :: Parser String
moduleDecl = do
  keyword "module"
  name <- identifier <?> "module name"
  scn
  return name

program :: Parser Program
program = do
  scn
  moduleName' <- optional moduleDecl
  exprs' <- lines'
  eof
  return $ Program exprs' moduleName'

var :: Parser Expr
var = do
  start' <- getOffset
  name <- extra <|> gravis
  Var name . position start' <$> getOffset

target :: Parser Expr
target = do
  start <- getOffset
  symbol "__target"
  target' <- (symbol "wasm" <|> symbol "c") <?> "target"
  expr' <- expr <?> "target"
  end <- getOffset
  return $ Target (unpack target') expr' (Position (start, end))

listPattern :: Parser Expr
listPattern = do
  start <- getOffset
  elements <- sepBy1 (var <|> placeholder <|> array) (symbol ":") <?> "list pattern"
  end <- getOffset
  return $ ListPattern elements (Position (start, end))

lambda :: Parser Expr
lambda = do
  start <- getOffset
  symbol "\\"
  args <- some (var <|> parens listPattern <|> array <|> placeholder) <?> "lambda arguments"
  symbol "->"
  body <- expr <?> "lambda body"
  end <- getOffset
  return $ Lambda args body (Position (start, end))

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
      return $ Placeholder anyPosition

typeLiteral :: Parser Expr
typeLiteral = do
  start <- getOffset
  t <- validType
  end <- getOffset
  return $ TypeLit t (Position (start, end))

trait :: Parser Expr
trait = do
  start <- getOffset
  keyword "trait"
  name <- identifier <?> "trait name"
  generics <- fromMaybe [] <$> optional generic <?> "trait generics"
  hasEquals <- lookAhead (optional (try (symbol "=")))
  (requiredProperties, refinementSrc, refinement, methods) <- case hasEquals of
    Just _ -> do
      symbol "="
      reqProps <- optional (try (parens (traitField `sepBy` symbol ",")) <?> "required properties")
      refSrc <- lookAhead $ optional $ do
        keyword "satisfies"
        parens (many (noneOf [')'])) <?> "refinement source"
      ref <- optional $ do
        keyword "satisfies"
        parens expr <?> "refinement"
      methods' <-
        ( do
            keyword "do"
            newline'
            fds <- funcDec `sepEndBy` newline' <?> "trait methods"
            keyword "end"
            return fds
          )
          <|> return []
      return (reqProps, refSrc, ref, methods')
    Nothing -> do
      reqProps <- optional (try (parens (traitField `sepBy` symbol ",")) <?> "required properties")
      refSrc <- lookAhead $ optional $ do
        keyword "satisfies"
        parens (many (noneOf [')'])) <?> "refinement source"
      ref <- optional $ do
        keyword "satisfies"
        parens expr <?> "refinement"
      hasEqualsAfter <- lookAhead (optional (try (symbol "=")))
      methods' <- case hasEqualsAfter of
        Just _ -> do
          symbol "="
          ( do
              keyword "do"
              newline'
              fds <- funcDec `sepEndBy` newline' <?> "trait methods"
              keyword "end"
              return fds
            )
            <|> return []
        Nothing -> return []
      return (reqProps, refSrc, ref, methods')
  end <- getOffset
  return $ Trait name methods generics (fromMaybe [] requiredProperties) refinement (fromMaybe "" refinementSrc) (Position (start, end))
  where
    traitField = do
      fieldName <- identifier <?> "field name"
      symbol ":"
      fieldType <- validType <?> "field type"
      return (fieldName, fieldType)

impl :: Parser Expr
impl = do
  start <- getOffset
  keyword "impl"
  name <- identifier <?> "impl name"
  traitTypeArgs <- optional $ do
    symbol "<"
    args <- sepBy1 validType (symbol ",") <?> "trait type arguments"
    symbol ">"
    return args
  symbol "for"
  for <- validType <?> "impl for"
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
  end <- getOffset
  return $ Impl name (fromMaybe [] traitTypeArgs) for methods (Position (start, end))

external :: Parser Expr
external = do
  start <- getOffset
  keyword "external"
  from <- stringLit <?> "external from"
  symbol "="
  symbol "do"
  newline'
  decs <- funcDec `sepEndBy` newline' <?> "external declarations"
  symbol "end"
  end <- getOffset
  return $ External from decs (Position (start, end))

unaryMinus :: Parser Expr
unaryMinus = parens $ do
  start <- getOffset
  symbol "-"
  expr' <- expr <?> "negatable expression"
  end <- getOffset
  return $ UnaryMinus expr' (Position (start, end))

tupleLit :: Parser Expr
tupleLit = do
  start <- getOffset
  symbol "("
  firstExpr <- expr
  hasCommaAfterFirst <- optional (symbol ",")
  case hasCommaAfterFirst of
    Just _ -> do
      nextIsClose <- lookAhead (optional (symbol ")"))
      case nextIsClose of
        Just _ -> do
          symbol ")"
          end <- getOffset
          return $ TupleLit [firstExpr] (Position (start, end))
        Nothing -> do
          restExprs <- sepBy expr (symbol ",")
          optional (symbol ",")
          symbol ")"
          end <- getOffset
          return $ TupleLit (firstExpr : restExprs) (Position (start, end))
    Nothing -> fail "Tuple must have at least 2 elements or a trailing comma for single-element tuple"

term :: Parser Expr
term =
  choice
    [ placeholder,
      try unaryMinus,
      try parenApply,
      try tupleLit,
      parens expr,
      do
        start <- getOffset
        val <- try charLit
        end <- getOffset
        return $ CharLit val (Position (start, end)),
      do
        start <- getOffset
        val <- try double
        end <- getOffset
        return $ DoubleLit val (Position (start, end)),
      do
        start <- getOffset
        val <- try float
        end <- getOffset
        return $ FloatLit val (Position (start, end)),
      do
        start <- getOffset
        val <- try integer
        end <- getOffset
        return $ IntLit val (Position (start, end)),
      do
        start <- getOffset
        val <- try stringLit
        end <- getOffset
        return $ StringLit val (Position (start, end)),
      do
        start <- getOffset
        symbol "True"
        end <- getOffset
        return $ BoolLit True (Position (start, end)),
      do
        start <- getOffset
        symbol "False"
        end <- getOffset
        return $ BoolLit False (Position (start, end)),
      external,
      try valueType,
      try typeDef,
      import_,
      impl,
      Parser.trait,
      doBlock,
      try letExpr,
      try combinedFunc,
      try funcDef,
      try funcDec,
      try lambda,
      try structLit,
      array,
      try funcCall,
      try arrayAccess,
      try whenExprParser,
      ifExpr,
      var
    ]
