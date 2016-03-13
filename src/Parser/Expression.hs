module Parser.Expression (
  expr
) where

import Control.Monad.State
import Control.Applicative ((<$>), (<*>))
import Text.Parsec hiding (parse, State)
import Text.Parsec.Indent
import qualified Text.Parsec.Expr as Ex

import Parser.Parser
import qualified Parser.Lexer as L
import Syntax



expr :: Parser Expr
expr = Ex.buildExpressionParser operatorTable exprWithoutOps
  where
    exprWithoutOps :: Parser Expr
    exprWithoutOps = sameOrIndented >> (try float
                                    <|> try integer
                                    <|> try bool
                                    <|> try ifThenElse
                                    <|> try call
                                    <|> try variable
                                    <|> L.parens expr)
    operatorTable :: [[Ex.Operator String () (State SourcePos) Expr]]
    operatorTable = [
        [reservedUnaryOp "-"],
        [reservedBinOp "*",
         reservedBinOp "/"],
        [reservedBinOp "+",
         reservedBinOp "-"],
        [userDefinedBinOp]
      ]
      where
        reservedUnaryOp :: String
                           -> Ex.Operator String () (State SourcePos) Expr
        reservedUnaryOp name
          = Ex.Prefix $ try $ do
            sameOrIndented
            L.reservedOp name
            return (\expr -> ECall ("unary." ++ name) [expr])

        reservedBinOp :: String -> Ex.Operator String () (State SourcePos) Expr
        reservedBinOp name
          = infixOp $ try $ do
            sameOrIndented
            L.reservedOp name
            return (callBinOpFunc name)

        userDefinedBinOp :: Ex.Operator String () (State SourcePos) Expr
        userDefinedBinOp
          = infixOp $ try $ do
            sameOrIndented
            callBinOpFunc <$> L.operator

        infixOp parser = Ex.Infix parser Ex.AssocLeft
        callBinOpFunc opName expr1 expr2
          = ECall ("binary." ++ opName) [expr1, expr2]

integer :: Parser Expr
integer = ENum <$> (fromInteger <$> L.integer)

float :: Parser Expr
float = ENum <$> L.float

variable :: Parser Expr
variable = EVar <$> L.identifier

call :: Parser Expr
call = ECall <$> L.identifier <*> (L.parens $ L.commaSep expr)

ifThenElse :: Parser Expr
ifThenElse = return EIf
        <-/> L.reserved "if" <+/> expr
        <-/> L.reserved "then" <+/> expr
        <-/> L.reserved "else" <+/> expr

bool :: Parser Expr
bool = true <|> false
  where
    true :: Parser Expr
    true = L.reserved "True" >> return (EBool True)
    false :: Parser Expr
    false = L.reserved "False" >> return (EBool False)
