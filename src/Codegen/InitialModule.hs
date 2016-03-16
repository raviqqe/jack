module Codegen.InitialModule (
  initialModule
) where

import LLVM.General.AST

import Codegen.ModuleMaker
import Codegen.Type



initialModule :: String -> Module
initialModule name = runModuleMaker (defaultModule { moduleName = name }) $ do
  declare float "unary.-" [(float, Name "x")]
  declare float "binary.+" [(float, Name "x"), (float, Name "y")]
  declare float "binary.-" [(float, Name "x"), (float, Name "y")]
  declare float "binary.*" [(float, Name "x"), (float, Name "y")]
  declare float "binary./" [(float, Name "x"), (float, Name "y")]

  declare (ptr byte) "malloc" [(int, Name "x")]
  declare void "free" [(ptr byte, Name "x")]
