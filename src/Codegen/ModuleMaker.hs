{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen.ModuleMaker (
  ModuleMaker,
  runModuleMaker,
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

addDefinition :: Definition -> ModuleMaker ()
addDefinition definition = do
  definitions <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = definitions ++ [definition] }

deleteDefinition :: String -> ModuleMaker ()
deleteDefinition funcName = do
  definitions <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = filter haveSameName definitions }
  where
    haveSameName :: Definition -> Bool
    haveSameName (GlobalDefinition (Function _ _ _ _ _ _ name _ _ _ _ _ _ _ _))
      | name == Name funcName = False
    haveSameName _ = True

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> ModuleMaker ()
define retType funcName args body = do
  deleteDefinition funcName
  addDefinition $ GlobalDefinition $ functionDefaults {
    name = Name funcName,
    parameters = ([Parameter argType name [] | (argType, name) <- args],
                  False),
    returnType = retType,
    basicBlocks = body
  }

declare :: Type -> String -> [(Type, Name)] -> ModuleMaker ()
declare retType funcName args = do
  deleteDefinition funcName
  addDefinition $ GlobalDefinition $ functionDefaults {
    name = Name funcName,
    parameters = ([Parameter argType name [] | (argType, name) <- args],
                  False),
    returnType = retType,
    basicBlocks = []
  }
