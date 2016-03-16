module Parser.Statement (
  statement
) where

import Control.Applicative ((<$>))
import Text.Parsec hiding (parse, State)
import Text.Parsec.Indent

import Name.Mangle
import Parser.Expression
import qualified Parser.Indent as I
import Parser.Lexer
import Parser.Parser
import Syntax



statement :: Parser Statement
statement = try extern <|> function
  where
    function :: Parser Statement
    function = withPos $ do
      signature <+/> withPos (reservedOp "=" >> sameOrIndented >> expr)
      where
        signature = try functionSignature
                <|> try binOpSignature
                <|> unaryOpSignature
        functionSignature = return STermDef
                       <+/> identifier
                       <+/> I.many identifier
        binOpSignature = return STermDef
                    <+/> (binaryOpFuncName <$> parens operator)
                    <+/> I.count 2 identifier
        unaryOpSignature = return STermDef
                      <+/> (unaryOpFuncName <$> parens operator)
                      <+/> I.count 1 identifier

    extern :: Parser Statement
    extern = withPos $ do
      reserved "import"
      return SImport <+/> identifier <+/> I.many identifier
