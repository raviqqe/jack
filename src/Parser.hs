module Parser (
  parseToplevels,
  parseStmts
) where

import Control.Applicative ((<$>))
import Data.Functor.Identity
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex

import Parser.Lexer
import Syntax


-- types

type Toplevel = Either Expr Stmt


-- functions

binOps :: [[Ex.Operator String () Data.Functor.Identity.Identity Expr]]
binOps = [[binary "*" Ex.AssocLeft,
            binary "/" Ex.AssocLeft],
           [binary "+" Ex.AssocLeft,
            binary "-" Ex.AssocLeft]]
  where
    binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

floating :: Parser Expr
floating = (return . Float) =<< float

expr :: Parser Expr
expr = Ex.buildExpressionParser binOps factor

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Stmt
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many identifier
  body <- expr
  return $ Function name args body

extern :: Parser Stmt
extern = do
  reserved "extern"
  name <- identifier
  args <- parens $ many identifier
  return $ Extern name args

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

factor :: Parser Expr
factor = try floating
     <|> try int
     <|> try call
     <|> variable
     <|> parens expr

contents :: Parser a -> Parser a
contents parser = do
  spaces
  c <- parser
  eof
  return c

statement :: Parser Stmt
statement = do
  s <- try extern <|> function
  reservedOp ";"
  return s

toplevelExpr :: Parser Expr
toplevelExpr = do
  e <- expr
  reservedOp ";"
  return e

toplevel :: Parser Toplevel
toplevel = try (toplevelExpr >>= (return . Left))
           <|> (statement    >>= (return . Right))

parseToplevels :: String -> String -> Either ParseError [Toplevel]
parseToplevels sourceName sourceCode
  = parse (contents $ many toplevel) sourceName sourceCode

parseStmts :: String -> String -> Either ParseError [Stmt]
parseStmts sourceName sourceCode
  = parse (contents $ many statement) sourceName sourceCode
