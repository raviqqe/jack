module Parser (
  Toplevel,
  ParseError,
  parseToplevel,
  parseToplevels,
  parseStmts,
  isExpr
) where

import Text.Parsec hiding (parse, State)
import Text.Parsec.Indent

import Parser.Expression
import Parser.Indent
import Parser.Statement
import Syntax



type Toplevel = Either Expr Stmt

contents :: Parser a -> Parser a
contents parser = do
  spaces
  c <- parser
  eof
  return c

toplevelExpr :: Parser Expr
toplevelExpr = withPos expr

toplevel :: Parser Toplevel
toplevel = try (toplevelExpr >>= (return . Left))
           <|> (statement    >>= (return . Right))

parseToplevel :: String -> String -> Either ParseError Toplevel
parseToplevel sourceName sourceCode
  = parse (contents $ toplevel) sourceName sourceCode

parseToplevels :: String -> String -> Either ParseError [Toplevel]
parseToplevels sourceName sourceCode
  = parse (contents $ many toplevel) sourceName sourceCode

parseStmts :: String -> String -> Either ParseError [Stmt]
parseStmts sourceName sourceCode
  = parse (contents $ many statement) sourceName sourceCode

isExpr :: Toplevel -> Bool
isExpr (Left _) = True
isExpr (Right _) = False
