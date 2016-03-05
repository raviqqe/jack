module Interpreter.JIT (
  runJIT
) where

import Foreign.Ptr ( FunPtr, castFunPtr )
import LLVM.General.Context
import LLVM.General.Module
import qualified LLVM.General.AST as AST
import LLVM.General.ExecutionEngine

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

withExecMod :: AST.Module -> (ExecutableModule MCJIT -> IO a) -> IO a
withExecMod astMod runExecMod = do
  withContext $ \context ->
    withMyJIT context $ \executionEngine ->
      liftExceptT $ withModuleFromAST context astMod $ \mod -> do
        withModuleInEngine executionEngine mod runExecMod

runJIT :: AST.Module -> IO ()
runJIT astMod = do
  withExecMod astMod $ \execMod -> do
    mainFuncPtr <- getFunction execMod (AST.Name "main")
    case mainFuncPtr of
      Just funcPtr -> do
        result <- callFuncPtr funcPtr
        putStrLn $ "Evaluated to: " ++ show result
      Nothing -> return ()
