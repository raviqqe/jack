module Parser (
  Toplevel,
  ParseError,
  parseToplevel,
  parseToplevels,
  parseStmts,
  isExpr
) where

import Control.Monad.State
import Control.Applicative ((<$>))
import Text.Parsec hiding (parse, State)
import Text.Parsec.Indent
import qualified Text.Parsec.Expr as Ex

import Parser.Indent
import Parser.Lexer
import Syntax


-- types

type Toplevel = Either Expr Stmt


-- functions

int :: Parser Expr
int = do
  n <- integer
  return $ Float (fromInteger n)

floating :: Parser Expr
floating = (return . Float) =<< float

expr :: Parser Expr
expr = Ex.buildExpressionParser operatorTable exprWithoutOps
  where
    exprWithoutOps :: Parser Expr
    exprWithoutOps = sameOrIndented >> (try floating
                                    <|> try int
                                    <|> try call
                                    <|> try variable
                                    <|> parens expr)
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
          reservedOp name
          return (BinaryOp name)

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Stmt
function = withPos $ do
  reserved "def"
  return Function <+/> identifier <+/> (parens $ many identifier) <+/> expr

extern :: Parser Stmt
extern = withPos $ do
  reserved "extern"
  return Extern <+/> identifier <+/> (parens $ many identifier)

call :: Parser Expr
call = do
  name <- identifier
  args <- parens $ commaSep expr
  return $ Call name args

contents :: Parser a -> Parser a
contents parser = do
  spaces
  c <- parser
  eof
  return c

statement :: Parser Stmt
statement = try extern <|> function

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

-- Utils

isExpr :: Toplevel -> Bool
isExpr (Left _) = True
isExpr (Right _) = False
