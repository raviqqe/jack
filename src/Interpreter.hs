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



-- Constatns

sourceName = "<stdin>"

-- REPL

type REPL = InputT IO

runREPL :: REPL () -> IO ()
runREPL = runInputT defaultSettings


-- functions

interpret :: IO ()
interpret = runREPL interpretInputLines

interpretInputLines :: REPL ()
interpretInputLines = do
  inputLine <- getInputLine "ready> "
  case inputLine of
    Nothing   -> outputStrLn "Goodbye."
    Just line -> interpretOneLine line >> interpretInputLines

interpretOneLine :: String -> REPL ()
interpretOneLine line = liftIO $ do
  case parseToplevels sourceName line of
    Left err -> print err
    Right expr -> mapM_ print expr


initModule :: AST.Module
initModule = emptyModule "Jack JIT REPL"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process astMod sourceCode
  = case parseToplevels sourceName sourceCode of
    Left err -> print err >> return Nothing
    Right toplevels -> return . Just =<< codegen astMod toplevels
