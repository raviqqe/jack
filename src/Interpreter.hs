module Interpreter (
  interpret
) where

import Control.Monad.Trans
import System.Console.Haskeline
import System.Environment
import System.IO
import qualified LLVM.General.AST as AST

import Parser
import Codegen



-- Constants

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
        --interpretOneLine line >> interpretInputLines
        maybeMod <- liftIO $ incorporateToplevels mod line
        makeModuleFromInputLines (case maybeMod of
          Just newMod -> newMod
          Nothing -> mod)

incorporateToplevels :: AST.Module -> String -> IO (Maybe AST.Module)
incorporateToplevels astMod sourceCode
  = case parseToplevels sourceName sourceCode of
    Left err -> print err >> return Nothing
    Right toplevels -> return . Just =<< codegen astMod toplevels
