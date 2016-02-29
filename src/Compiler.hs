module Compiler (
  compile
) where

import System.IO

import Parser
import Codegen
import Emit


-- Constants

stdinName = "<stdin>"


-- Functions

compile :: Maybe FilePath -> Maybe FilePath -> IO ()
compile (Just filename) _ = codegenSourceCode filename =<< readFile filename
compile Nothing _ = codegenSourceCode stdinName =<< getContents

codegenSourceCode :: FilePath -> String -> IO ()
codegenSourceCode sourceName sourceCode = do
  case parseToplevels sourceName sourceCode of
    Left err -> print err
    Right toplevels -> do
      codegen (emptyModule sourceName) toplevels
      return ()
