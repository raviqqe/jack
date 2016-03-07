{-# LANGUAGE FlexibleContexts #-}

module Parser.LanguageDef (
  languageDef
) where

import Text.Parsec
import Text.Parsec.Token



languageDef :: Stream s m Char => GenLanguageDef s u m
languageDef = LanguageDef {
    commentStart = "",
    commentEnd = "",
    commentLine = "#",
    nestedComments = False,
    identStart = letter <|> char '_',
    identLetter = alphaNum <|> char '_',
    opStart = opChar,
    opLetter = opChar,
    reservedNames = ["import", "if", "then", "else", "for"],
    reservedOpNames = ["+", "*", "-", "/", "="],
    caseSensitive = True
  }
  where
    opChar :: Stream s m Char => ParsecT s u m Char
    opChar = oneOf "+-*/^&|:;<=>!?\\~.@$%"
