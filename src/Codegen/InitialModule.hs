module Codegen.InitialModule (
  initialModule
) where

import Control.Monad hiding (void)
import LLVM.General.AST

import Codegen.ModuleMaker
import Codegen.Type
import Name.Mangle



initialModule :: String -> Module
initialModule name = runModuleMaker (defaultModule { moduleName = name }) $ do
  declare floatUnaryOpType (unaryOpFuncName "-") [Name "x"]
  forM_ ["+", "-", "*", "/"] $ \opName -> do
    declare floatBinOpType (binaryOpFuncName opName) [Name "x", Name "y"]

  declare (func (ptr byte) [int]) "malloc" [Name "x"]
  declare (func void [ptr byte])  "free"   [Name "x"]
  where
    floatUnaryOpType = func float [float]
    floatBinOpType = func float [float, float]
