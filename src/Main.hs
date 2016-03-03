module Main where

import System.IO

import Args
import Compiler
import Interpreter



main :: IO ()
main = do
  args <- parseArgs
  interpreterMode <- isInterpreterMode args
  if interpreterMode
    then interpret
    else compile (sourceCodeFilename args) (objectFilename args)
  where
    isInterpreterMode args = do
      stdinIsTerminal <- hIsTerminalDevice stdin
      return $ sourceCodeFilename args == Nothing && stdinIsTerminal
