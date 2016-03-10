module Parser.Statement (
  statement
) where

import Text.Parsec hiding (parse, State)
import Text.Parsec.Indent

import Parser.Expression
import Parser.Lexer
import Parser.Parser
import Syntax



statement :: Parser Stmt
statement = try extern <|> function
  where
    function :: Parser Stmt
    function = withPos $ do
      return Function <+/> identifier
                      <+/> many identifier
                      <+/> withPos (reservedOp "=" >> sameOrIndented >> expr)

    extern :: Parser Stmt
    extern = withPos $ do
      reserved "import"
      return Extern <+/> identifier <+/> many (sameOrIndented >> identifier)
