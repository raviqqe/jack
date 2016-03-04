{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen.ModuleMaker (
  ModuleMaker,
  runModuleMaker,
  emptyModule,
  define,
  declare
) where

import Control.Monad.State
import Control.Applicative
import LLVM.General.AST
import LLVM.General.AST.Global



newtype ModuleMaker a = ModuleMaker { unModuleMaker :: State Module a }
  deriving (Functor, Applicative, Monad, MonadState Module)

runModuleMaker :: Module -> ModuleMaker a -> Module
runModuleMaker = flip (execState . unModuleMaker)

emptyModule :: String -> Module
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
