module Main where

import System.IO

import Args
import Compiler
import Interpreter



main :: IO ()
main = do
  args <- parseArgs
  replMode <- isREPLMode args
  if replMode
    then interpret
    else compile (sourceCodeFilename args)
  where
    isREPLMode args = do
      stdinIsTerminal <- hIsTerminalDevice stdin
      return $ sourceCodeFilename args == Nothing && stdinIsTerminal
