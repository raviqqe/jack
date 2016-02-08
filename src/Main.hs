module Main where

import System.IO

import Args
import Compiler
import Interpreter



main :: IO ()
main = do
  args <- parseArgs
  interactiveMode <- hIsTerminalDevice stdin
  if sourceCodeFilename args == Nothing && interactiveMode
    then interpret
    else compile (sourceCodeFilename args)
