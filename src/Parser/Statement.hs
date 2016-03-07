module Parser.Statement (
  statement
) where

import Text.Parsec hiding (parse, State)
import Text.Parsec.Indent

import Parser.Expression
import Parser.Indent
import Parser.Lexer
import Syntax



statement :: Parser Stmt
statement = try extern <|> function
  where
    function :: Parser Stmt
    function = withPos $ do
      reserved "def"
      return Function <+/> identifier <+/> (parens $ many identifier) <+/> expr

    extern :: Parser Stmt
    extern = withPos $ do
      reserved "extern"
      return Extern <+/> identifier <+/> (parens $ many identifier)
