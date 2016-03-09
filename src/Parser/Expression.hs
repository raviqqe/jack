module Parser.Expression (
  expr
) where

import Control.Monad.State
import Control.Applicative ((<$>), (<*>))
import Text.Parsec hiding (parse, State)
import Text.Parsec.Indent
import qualified Text.Parsec.Expr as Ex

import Parser.Indent
import qualified Parser.Lexer as L
import Syntax



expr :: Parser Expr
expr = Ex.buildExpressionParser operatorTable exprWithoutOps
  where
    exprWithoutOps :: Parser Expr
    exprWithoutOps = sameOrIndented >> (try float
                                    <|> try integer
                                    <|> try ifThenElse
                                    <|> try call
                                    <|> try variable
                                    <|> L.parens expr)
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
          L.reservedOp name
          return (BinaryOp name)

integer :: Parser Expr
integer = Float <$> (fromInteger <$> L.integer)

float :: Parser Expr
float = Float <$> L.float

variable :: Parser Expr
variable = Var <$> L.identifier

call :: Parser Expr
call = Call <$> L.identifier <*> (L.parens $ L.commaSep expr)

ifThenElse :: Parser Expr
ifThenElse = return If
        <-/> L.reserved "if" <+/> expr
        <-/> L.reserved "then" <+/> expr
        <-/> L.reserved "else" <+/> expr
