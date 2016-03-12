module Codegen.InitialModule (
  initialModule
) where

import LLVM.General.AST

import Codegen.ModuleMaker
import Codegen.Type



initialModule :: String -> Module
initialModule name = runModuleMaker (defaultModule { moduleName = name }) $ do
  declare double "unary.-" [(double, Name "x")]
  declare double "binary.+" [(double, Name "x"), (double, Name "y")]
  declare double "binary.-" [(double, Name "x"), (double, Name "y")]
  declare double "binary.*" [(double, Name "x"), (double, Name "y")]
  declare double "binary./" [(double, Name "x"), (double, Name "y")]
