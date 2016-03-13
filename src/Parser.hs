module Parser (
  Toplevel,
  ParseError,
  parseToplevel,
  parseToplevels,
  parseStatements,
  isExpr
) where

import Text.Parsec hiding (parse, State)
import Text.Parsec.Indent

import Parser.Expression
import Parser.Parser
import Parser.Statement
import Syntax



type Toplevel = Either Expr Statement

contents :: Parser a -> Parser a
contents parser = do
  spaces
  c <- parser
  eof
  return c

toplevelExpr :: Parser Expr
toplevelExpr = withPos expr

toplevel :: Parser Toplevel
toplevel = try (statement    >>= (return . Right))
           <|> (toplevelExpr >>= (return . Left))

parseToplevel :: String -> String -> Either ParseError Toplevel
parseToplevel sourceName sourceCode
  = parse (contents $ toplevel) sourceName sourceCode

parseToplevels :: String -> String -> Either ParseError [Toplevel]
parseToplevels sourceName sourceCode
  = parse (contents $ many toplevel) sourceName sourceCode

parseStatements :: String -> String -> Either ParseError [Statement]
parseStatements sourceName sourceCode
  = parse (contents $ many statement) sourceName sourceCode

isExpr :: Toplevel -> Bool
isExpr (Left _) = True
isExpr (Right _) = False
