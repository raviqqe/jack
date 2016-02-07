module Compiler (
  compile
) where

import System.IO



compile :: Maybe FilePath -> IO ()
compile maybeFilename = do
  sourceCode <- case maybeFilename of
    Nothing -> getContents
    Just filename -> readFile filename
  putStr sourceCode -- DEBUG
  --let theModule = compile sourceCode
  --outputModuleToFile theModule filename
