module Codegen.InitialModule (
  initialModule
) where

import Control.Monad hiding (void)
import LLVM.General.AST

import Codegen.ModuleMaker
import Codegen.Type
import Constant
import Name.Mangle



initialModule :: String -> Module
initialModule name = runModuleMaker (defaultModule { moduleName = name }) $ do
  typeDef closureTypeName $ struct [ptr byte, ptr byte]

  declare floatUnaryOpType (unaryOpFuncName "-") ["x"]
  forM_ ["+", "-", "*", "/"] $ \opName -> do
    declare floatBinOpType (binaryOpFuncName opName) ["x", "y"]

  declare (func (ptr byte) [int]) "malloc" ["x"]
  declare (func void [ptr byte])  "free"   ["x"]

  declare (func (typeRef closureTypeName) []) "dummy_to_keep_closure_type" []
  where
    floatUnaryOpType = func float [float]
    floatBinOpType = func float [float, float]
