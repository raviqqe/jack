module Parser.Statement (
  statement
) where

import Control.Applicative ((<$>))
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
      signature <+/> withPos (reservedOp "=" >> sameOrIndented >> expr)
      where
        signature = try functionSignature
                <|> try binOpSignature
                <|> unaryOpSignature
        functionSignature = return Function
                       <+/> identifier
                       <+/> many (sameOrIndented >> identifier)
        binOpSignature = return Function
                    <+/> (("binary." ++) <$> parens operator)
                    <+/> count 2 (sameOrIndented >> identifier)
        unaryOpSignature = return Function
                      <+/> (("unary." ++) <$> parens operator)
                      <+/> count 1 (sameOrIndented >> identifier)

    extern :: Parser Stmt
    extern = withPos $ do
      reserved "import"
      return Extern <+/> identifier <+/> many (sameOrIndented >> identifier)
