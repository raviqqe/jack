module Compiler (
  compile
) where

import System.IO


-- DEBUG
compile :: Maybe FilePath -> IO ()
compile (Just filename) = putStr =<< readFile filename
compile Nothing = putStr =<< getContents
  --let theModule = compile sourceCode
  --outputModuleToFile theModule outputFilename
