{-# LANGUAGE RankNTypes #-}

module Interpreter (
  interpret
) where

import Control.Monad
import Control.Monad.State
import System.Console.Haskeline

import Parser
import Codegen
import Interpreter.Eval



type Interpreter = StateT Module (InputT IO)

runInterpreter :: Interpreter () -> IO ()
runInterpreter interpreter
  = runInputT defaultSettings
              (evalStateT interpreter (initialModule "Jack Interpreter"))

interpret :: IO ()
interpret = runInterpreter interpretInputLines
  where
    interpretInputLines :: Interpreter ()
    interpretInputLines = do
      inputLines <- lift $ getInputLines
      case inputLines of
        Nothing   -> lift $ outputStrLn "Goodbye."
        Just lines -> do
          processResult (parseToplevel sourceName lines)
          interpretInputLines
      where
        processResult :: Either ParseError Toplevel -> Interpreter ()
        processResult (Left err) = liftIO $ print err
        processResult (Right toplevel) = do
          oldMod <- get
          newMod <- liftIO $ codegen oldMod [toplevel]
          put newMod
          liftIO $ putStrLn =<< assemblyFromModule newMod
          when (isExpr toplevel) $ do
            result <- liftIO $ eval newMod
            lift $ outputStrLn ("Evaluated to: " ++ result)

getInputLines :: forall m . MonadException m => InputT m (Maybe String)
getInputLines = promptInputLines (">>> ":repeat "... ") ""
  where
    promptInputLines :: forall m . MonadException m =>
                        [String] -> String -> InputT m (Maybe String)
    promptInputLines (prompt:prompts) oldLines = do
      inputLine <- getInputLine prompt
      case inputLine of
        Nothing -> return Nothing
        Just "" -> return (Just oldLines)
        Just line -> promptInputLines prompts (oldLines ++ "\n" ++ line)
    promptInputLines [] _ = fail "Prompts must be an infinite-length list of \
                                 \strings."

sourceName :: String
sourceName = "<stdin>"
