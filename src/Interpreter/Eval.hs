module Interpreter.Eval (
  eval
) where

import Foreign.Ptr ( FunPtr, castFunPtr )
import LLVM.General.Context
import LLVM.General.AST
import LLVM.General.ExecutionEngine
import qualified LLVM.General.Module as M

import Util



foreign import ccall "dynamic" haskFunc :: FunPtr (IO Double) -> (IO Double)

callFuncPtr :: FunPtr a -> IO Double
callFuncPtr funcPtr = haskFunc (castFunPtr funcPtr :: FunPtr (IO Double))

withMyJIT :: Context -> (MCJIT -> IO a) -> IO a
withMyJIT context
  = withMCJIT context optLevel codeModel framePtrElim fastInstrSelect
  where
    optLevel        = Just 0
    codeModel       = Nothing
    framePtrElim    = Nothing
    fastInstrSelect = Nothing

withExecMod :: Module -> (ExecutableModule MCJIT -> IO a) -> IO a
withExecMod mod runExecMod = do
  withContext $ \context ->
    withMyJIT context $ \executionEngine ->
      liftExceptT $ M.withModuleFromAST context mod $ \modObj -> do
        withModuleInEngine executionEngine modObj runExecMod

eval :: Module -> IO String
eval mod = do
  withExecMod mod $ \execMod -> do
    mainFuncPtr <- getFunction execMod (Name evaluatedFuncName)
    case mainFuncPtr of
      Just funcPtr -> do
        result <- callFuncPtr funcPtr
        return (show result)
      Nothing -> fail ("The function, \"" ++ evaluatedFuncName
                       ++ "\" to be evaluated was not found.")
  where
    evaluatedFuncName = "main"
