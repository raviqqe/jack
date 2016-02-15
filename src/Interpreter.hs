module Interpreter (
  interpret
) where

import Control.Monad.Trans
import System.Console.Haskeline
import System.IO

import Parser



interpret :: IO ()
interpret = runInputT defaultSettings interpretInputLines
  where
    interpretInputLines :: InputT IO ()
    interpretInputLines = do
      maybeInputLine <- getInputLine "ready> "
      processInputLine maybeInputLine

    processInputLine :: Maybe String -> InputT IO ()
    processInputLine (Just line) = do
      liftIO $ interpretOneLine line
      interpretInputLines
    processInputLine Nothing = outputStrLn "Goodbye."


interpretOneLine :: String -> IO ()
interpretOneLine line = do
  case parseToplevels "<stdin>" line of
    Left err -> print err
    Right expr -> mapM_ print expr
