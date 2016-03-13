{-# LANGUAGE FlexibleContexts #-}

module Parser.Indent (
  many,
  many1,
  count
) where

import Control.Monad.State
import qualified Text.Parsec as P
import Text.Parsec.Indent



many :: P.Stream s (State P.SourcePos) t =>
        IndentParser s u a -> IndentParser s u [a]
many parser = P.many (sameOrIndented >> parser)

many1 :: P.Stream s (State P.SourcePos) t =>
         IndentParser s u a -> IndentParser s u [a]
many1 parser = P.many1 (sameOrIndented >> parser)

count :: P.Stream s (State P.SourcePos) t =>
         Int -> IndentParser s u a -> IndentParser s u [a]
count num parser = P.count num (sameOrIndented >> parser)
