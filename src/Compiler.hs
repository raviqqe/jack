module Compiler (
  compile
) where

import System.IO


compile :: Maybe FilePath -> Maybe FilePath -> IO ()
compile (Just filename) _ = putStr =<< readFile filename
compile Nothing _ = putStr =<< getContents
--let theModule = compile sourceCode
--outputModuleToFile theModule outputFilename


--sourceToModule
