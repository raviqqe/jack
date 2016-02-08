module Interpreter (
  interpret
) where

import Control.Monad.Trans
import System.Console.Haskeline
import System.IO

import Parser
import Lexer



interpret :: IO ()
interpret = runInputT defaultSettings interpretInputLines
  where
    interpretInputLines = do
      maybeInput <- getInputLine "ready> "
      case maybeInput of
        Nothing -> outputStrLn "Goodbye."
        Just input -> do
          liftIO $ interpretOneLine input
          interpretInputLines


interpretOneLine :: String -> IO ()
interpretOneLine line = do
  case parseToplevel line of
    Left err -> print err
    Right expr -> mapM_ print expr
