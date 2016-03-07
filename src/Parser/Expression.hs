module Parser.Expression (
  expr
) where

import Control.Monad.State
import Control.Applicative ((<$>))
import Text.Parsec hiding (parse, State)
import Text.Parsec.Indent
import qualified Text.Parsec.Expr as Ex

import Parser.Indent
import Parser.Lexer
import Syntax



expr :: Parser Expr
expr = Ex.buildExpressionParser operatorTable exprWithoutOps
  where
    exprWithoutOps :: Parser Expr
    exprWithoutOps = sameOrIndented >> (try floating
                                    <|> try int
                                    <|> try call
                                    <|> try variable
                                    <|> parens expr)
    operatorTable :: [[Ex.Operator String () (State SourcePos) Expr]]
    operatorTable = [
        [binary "*" Ex.AssocLeft,
         binary "/" Ex.AssocLeft],
        [binary "+" Ex.AssocLeft,
         binary "-" Ex.AssocLeft]
      ]
      where
        binary :: String -> Ex.Assoc
                  -> Ex.Operator String () (State SourcePos) Expr
        binary name = Ex.Infix $ do
          sameOrIndented
          reservedOp name
          return (BinaryOp name)

int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

floating :: Parser Expr
floating = (return . Float) =<< float

variable :: Parser Expr
variable = Var <$> identifier

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args
