module Parser.Parser (
  parse,
  Parser
) where

import qualified Text.Parsec as P
import Text.Parsec.Indent



type Parser a = IndentParser String () a

parse :: Parser a -> String -> String -> Either P.ParseError a
parse parser sourceName sourceCode
  = runIndent sourceName $ P.runParserT parser () sourceName sourceCode
