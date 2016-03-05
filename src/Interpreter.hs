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
              (evalStateT interpreter (emptyModule "Jack Interpreter"))

interpret :: IO ()
interpret = runInterpreter interpretInputLines
  where
    interpretInputLines :: Interpreter ()
    interpretInputLines = do
      inputLine <- lift $ getInputLine "ready> "
      case inputLine of
        Nothing   -> lift $ outputStrLn "Goodbye."
        Just line -> do
          processResult (parseToplevel sourceName line)
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

sourceName :: String
sourceName = "<stdin>"
