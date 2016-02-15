module Parser (
  parseToplevels
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex

import Lexer
import Syntax



opTable = [[binary "*" Times  Ex.AssocLeft,
            binary "/" Divide Ex.AssocLeft],
           [binary "+" Plus   Ex.AssocLeft,
            binary "-" Minus  Ex.AssocLeft]]
  where
    binary s f assoc = Ex.Infix (reservedOp s >> return (BinOp f)) assoc

int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

floating :: Parser Expr
floating = (return . Float) =<< float

expr :: Parser Expr
expr = Ex.buildExpressionParser opTable factor

variable :: Parser Expr
variable = do
  var <- identifier
  return $ Var var

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many variable
  body <- expr
  return $ Function name args body

extern :: Parser Expr
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many variable
  return $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

factor :: Parser Expr
factor = try floating
     <|> try int
     <|> try extern
     <|> try function
     <|> try call
     <|> variable
     <|> parens expr

contents :: Parser a -> Parser a
contents parser = do
  spaces
  c <- parser
  eof
  return c

statement :: Parser Expr
statement = try extern <|> function

toplevel :: Parser Expr
toplevel = do
  t <- try statement <|> expr
  reservedOp ";"
  return t

parseExpr :: String -> String -> Either ParseError Expr
parseExpr sourceName sourceCode = parse (contents expr) sourceName sourceCode

parseToplevels :: String -> String -> Either ParseError [Expr]
parseToplevels sourceName sourceCode
  = parse (contents $ many toplevel) sourceName sourceCode
