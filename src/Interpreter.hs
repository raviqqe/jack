module Interpreter (
  interpret
) where

import Control.Monad
import Control.Monad.Trans
import System.Console.Haskeline

import Parser
import Codegen
import Interpreter.Eval



-- Interpreter

type Interpreter = InputT IO

runInterpreter :: Interpreter () -> IO ()
runInterpreter = runInputT defaultSettings


-- functions

interpret :: IO ()
interpret = runInterpreter (makeModuleFromInputLines initModule)
  where
    makeModuleFromInputLines :: Module -> Interpreter ()
    makeModuleFromInputLines mod = do
      inputLine <- getInputLine "ready> "
      case inputLine of
        Nothing   -> outputStrLn "Goodbye."
        Just line -> do
          newMod <- processResult mod (parseToplevel sourceName line)
          makeModuleFromInputLines newMod
      where
        processResult :: Module -> Either ParseError Toplevel
                         -> Interpreter Module
        processResult mod (Right toplevel) = do
          newMod <- liftIO $ codegen mod [toplevel]
          liftIO $ putStrLn =<< assemblyFromModule newMod
          when (isExpr toplevel) $ do
            result <- liftIO $ eval newMod
            outputStrLn ("Evaluated to: " ++ result)
          return newMod
        processResult mod (Left err) = do
          liftIO $ print err
          return mod

initModule :: Module
initModule = emptyModule "Jack Interpreter"

sourceName :: String
sourceName = "<stdin>"
