module Parser (parseProgram) where

import Expr

import Control.Monad (guard)
import Text.Parsec hiding (State)
import Text.Parsec.String
import qualified Data.Set as S

-- Parsing
parseProgram :: String -> Either ParseError Expr
parseProgram = parse (parseExpr <* eof) "LETREC"

parseExpr :: Parser Expr
parseExpr = many space *> (
  parseInt
  <|> try parseLetStar
  <|> try parseLet
  <|> try parseApply
  <|> try parseVar
  <|> try parseCons
  <|> try parseLetProc
  <|> parseProc
  <|> parseUnpack
  <|> parseList
  <|> parseListUnOp
  <|> parseArithBinOp
  <|> parseArithBinPred
  <|> parseIsZero
  <|> parseMinus
  <|> parseIf
  ) <* many space

parseInt :: Parser Expr
parseInt = I . read <$> (
  try ((:) <$> char '-' <*> many1 digit)
  <|> many1 digit)

parseLetStar :: Parser Expr
parseLetStar = LetStar <$> (tok "let*" *> many1 (try parseAttrib)) <*> (tok "in" *> parseExpr)

parseLet :: Parser Expr
parseLet = Let <$> (tok "let" *> many1 (try parseAttrib)) <*> (tok "in" *> parseExpr)

parseVar :: Parser Expr
parseVar = Var <$> parseIdentifier reserved

parseCons :: Parser Expr
parseCons = parseBinOp "cons" Cons

parseLetProc :: Parser Expr
parseLetProc = parseFunctionLike LetProc "letproc"

parseUnpack :: Parser Expr
parseUnpack = Unpack
  <$> (tok "unpack" *> many1 (parseIdentifier reserved <* many space))
  <*> (symbol '=' *> parseExpr)
  <*> (tok "in" *> parseExpr)

parseProc :: Parser Expr
parseProc = Proc
  <$> (func "proc" *> betweenParens (sepBy (parseIdentifier reserved) (symbol ',')))
  <*> parseExpr

parseApply :: Parser Expr
parseApply = Apply
  <$> parseIdentifier reserved
  <*> betweenParens (sepBy parseExpr (symbol ','))

parseList :: Parser Expr
parseList = try parseEmptyList <|> parseNonEmptyList

parseEmptyList, parseNonEmptyList :: Parser Expr
parseEmptyList = const Nil <$> string "emptylist"

parseNonEmptyList =
  foldr Cons Nil <$> (func "list" *> betweenParens (sepBy1 parseExpr (char ',')))

parseListUnOp :: Parser Expr
parseListUnOp =
  try parseCar
  <|> try parseCdr
  <|> parseNull

parseCar, parseCdr, parseNull :: Parser Expr
parseCar = parseUnaryListOp "car" head
parseCdr = parseUnaryListOp "cdr" (VList . tail)
parseNull = parseUnaryListOp "null?" (VBool . null)

parseArithBinOp :: Parser Expr
parseArithBinOp = parseDiff
  <|> parseAdd
  <|> parseMul
  <|> parseDiv

parseDiff, parseAdd, parseMul, parseDiv :: Parser Expr
parseDiff = parseBinOp "-" (BinOp "-" (liftIntOp (-) VInt))
parseAdd  = parseBinOp "+" (BinOp "+" (liftIntOp (+) VInt))
parseMul  = parseBinOp "*" (BinOp "*" (liftIntOp (*) VInt))
parseDiv  = parseBinOp "/" (BinOp "/" (liftIntOp div VInt))

parseArithBinPred :: Parser Expr
parseArithBinPred =
  parseEqual
  <|> parseGreater
  <|> parseLess

parseEqual, parseGreater, parseLess :: Parser Expr
parseEqual = parseBinOp "equal?" (BinOp "equal?" (liftIntOp (==) VBool))
parseGreater = parseBinOp "greater?" (BinOp "greater?" (liftIntOp (>) VBool))
parseLess = parseBinOp "less?" (BinOp "less?" (liftIntOp (<) VBool))

parseIsZero :: Parser Expr
parseIsZero = parseUnaryIntOp "zero?" (VBool . (== 0))

parseMinus :: Parser Expr
parseMinus = parseUnaryIntOp "minus" (VInt . negate)

parseIf :: Parser Expr
parseIf = If
  <$> (tok "if"   *> parseExpr)
  <*> (tok "then" *> parseExpr)
  <*> (tok "else" *> parseExpr)

-- Parse utils
func :: String -> Parser String
func name = string name <* many space

tok :: String -> Parser String
tok s = string s <* many1 space

symbol :: Char -> Parser Char
symbol c = char c <* many space

betweenParens :: Parser a -> Parser a
betweenParens = between (char '(' <* many space) (char ')' <* many space)

parseOperands :: Parser (Expr, Expr)
parseOperands = do
  a <- parseExpr
  b <- symbol ',' *> parseExpr
  pure (a, b)

parseAttrib :: Parser (String, Expr)
parseAttrib =
  (,) <$> (parseIdentifier reserved <* many space) <*> (symbol '=' *> parseExpr)

parseBinOp :: String -> (Expr -> Expr -> Expr) -> Parser Expr
parseBinOp sym op = func sym *> (uncurry op <$> betweenParens parseOperands)

parseUnaryListOp :: String -> ([Val] -> Val) -> Parser Expr
parseUnaryListOp name f =
  ListUnOp name f <$> (func name *> betweenParens parseExpr)

liftIntOp :: (Int -> Int -> a) -> (a -> Val) -> (Int -> Int -> Val)
liftIntOp op cons = (cons .) . op

parseUnaryIntOp :: String -> (Int -> Val) -> Parser Expr
parseUnaryIntOp name f = UnOp name f <$> (func name *> betweenParens parseExpr)

parseIdentifier :: S.Set String -> Parser String
parseIdentifier reservedNames = do
  first <- letter
  rest <- many alphaNum <* many space
  let name = first : rest
  guard $ name `notElem` reservedNames
  pure name

parseFunctionLike :: (String -> [String] -> Expr -> Expr -> Expr) -> String -> Parser Expr
parseFunctionLike constructor constructorString = constructor
  <$> (tok constructorString *> parseIdentifier reserved <* many space)
  <*> betweenParens (sepBy (parseIdentifier reserved) (symbol ','))
  <*> (symbol '=' *> parseExpr)
  <*> (tok "in" *> parseExpr)

reserved :: S.Set String
reserved = S.fromList [
  "car",
  "cdr",
  "cons",
  "else",
  "emptylist",
  "equal",
  "greater",
  "if",
  "in",
  "less",
  "let",
  "letproc",
  "letrec",
  "list",
  "minus",
  "null",
  "proc",
  "then",
  "unpack",
  "zero"
  ]
