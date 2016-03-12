module Compiler (
  compile
) where

import Parser
import Codegen


-- Constants

stdinName :: String
stdinName = "<stdin>"


-- Functions

compile :: Maybe FilePath -> Maybe FilePath -> IO ()
compile (Just filename) _ = compileSourceCode filename =<< readFile filename
compile Nothing _ = compileSourceCode stdinName =<< getContents

compileSourceCode :: FilePath -> String -> IO ()
compileSourceCode sourceName sourceCode = do
  case parseToplevels sourceName sourceCode of
    Left err -> print err
    Right toplevels -> do
      mod <- codegen (initialModule sourceName) toplevels
      putStrLn =<< assemblyFromModule mod
