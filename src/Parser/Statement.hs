module Parser.Statement (
  statement
) where

import Control.Applicative ((<$>))
import Text.Parsec hiding (parse, State)
import Text.Parsec.Indent

import Parser.Expression
import qualified Parser.Indent as I
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
                       <+/> I.many identifier
        binOpSignature = return Function
                    <+/> (("binary." ++) <$> parens operator)
                    <+/> I.count 2 identifier
        unaryOpSignature = return Function
                      <+/> (("unary." ++) <$> parens operator)
                      <+/> I.count 1 identifier

    extern :: Parser Stmt
    extern = withPos $ do
      reserved "import"
      return Extern <+/> identifier <+/> I.many identifier
