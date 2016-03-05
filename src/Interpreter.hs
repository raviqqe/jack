module Interpreter (
  interpret
) where

import Control.Monad.Trans
import System.Console.Haskeline
import qualified LLVM.General.AST as AST

import Parser
import Codegen
import Interpreter.JIT



-- Constants

sourceName :: String
sourceName = "<stdin>"


-- Interpreter

type Interpreter = InputT IO

runInterpreter :: Interpreter () -> IO ()
runInterpreter = runInputT defaultSettings


-- functions

initModule :: AST.Module
initModule = emptyModule "Jack Interpreter"

interpret :: IO ()
interpret = runInterpreter (makeModuleFromInputLines initModule)
  where
  makeModuleFromInputLines :: AST.Module -> Interpreter ()
  makeModuleFromInputLines mod = do
    inputLine <- getInputLine "ready> "
    case inputLine of
      Nothing   -> outputStrLn "Goodbye."
      Just line -> do
        maybeMod <- liftIO $ incorporateToplevels mod line
        case maybeMod of
          Just newMod -> do
            liftIO $ putStrLn =<< assemblyFromModule newMod
            liftIO $ runJIT newMod
            makeModuleFromInputLines newMod
          Nothing -> makeModuleFromInputLines mod

incorporateToplevels :: AST.Module -> String -> IO (Maybe AST.Module)
incorporateToplevels astMod sourceCode
  = case parseToplevels sourceName sourceCode of
    Left err -> print err >> return Nothing
    Right toplevels -> return . Just =<< codegen astMod toplevels
