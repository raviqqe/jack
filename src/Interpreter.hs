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
import Emit



-- Constants

sourceName = "<stdin>"


-- REPL

type REPL = InputT IO

runREPL :: REPL () -> IO ()
runREPL = runInputT defaultSettings


-- functions

initModule :: AST.Module
initModule = emptyModule "Jack JIT REPL"

interpret :: IO ()
interpret = runREPL (makeModuleFromInputLines initModule)
  where
  makeModuleFromInputLines :: AST.Module -> REPL ()
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
