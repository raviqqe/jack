{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen.ModuleMaker where

import Control.Monad.State
import Control.Applicative
import Data.Function
import Data.List
import Data.String
import Data.Word
import qualified Data.Map as Map
import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import qualified NameSupply as NS



newtype ModuleMaker a = ModuleMaker { unModuleMaker :: State AST.Module a }
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

runModuleMaker :: AST.Module -> ModuleMaker a -> AST.Module
runModuleMaker = flip (execState . unModuleMaker)

emptyModule :: String -> AST.Module
emptyModule name = defaultModule { moduleName = name }

addDefinition :: Definition -> ModuleMaker ()
addDefinition definition = do
  definitions <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = definitions ++ [definition] }

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> ModuleMaker ()
define retType functionName args body = addDefinition $
  GlobalDefinition $ functionDefaults {
    name = Name functionName,
    parameters = ([Parameter argType name [] | (argType, name) <- args],
                  False),
    returnType = retType,
    basicBlocks = body
  }

declare :: Type -> String -> [(Type, Name)] -> ModuleMaker ()
declare retType label args = addDefinition $
  GlobalDefinition $ functionDefaults {
    name = Name label,
    parameters = ([Parameter argType name [] | (argType, name) <- args],
                  False),
    returnType = retType,
    basicBlocks = []
  }
