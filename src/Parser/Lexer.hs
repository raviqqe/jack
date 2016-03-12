{-# LANGUAGE FlexibleContexts #-}

module Parser.Lexer (
  integer,
  float,
  parens,
  commaSep,
  semiSep,
  identifier,
  operator,
  reserved,
  reservedOp
)where

import Text.Parsec hiding (parse, State)
import qualified Text.Parsec.Token as Tok

import Parser.Parser
import Parser.LanguageDef



lexer :: Stream s m Char => Tok.GenTokenParser s u m
lexer = Tok.makeTokenParser languageDef

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

operator :: Parser String
operator = Tok.operator lexer
