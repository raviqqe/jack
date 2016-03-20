{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen.ModuleMaker (
  ModuleMaker,
  runModuleMaker,
  define,
  declare,
  typeDef
) where

import Control.Monad.State
import Control.Applicative
import LLVM.General.AST
import LLVM.General.AST.Global

import Codegen.Type



newtype ModuleMaker a = ModuleMaker { unModuleMaker :: State Module a }
  deriving (Functor, Applicative, Monad, MonadState Module)

runModuleMaker :: Module -> ModuleMaker a -> Module
runModuleMaker = flip (execState . unModuleMaker)

addDefinition :: Definition -> ModuleMaker ()
addDefinition definition = do
  definitions <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = definitions ++ [definition] }

deleteDefinition :: String -> ModuleMaker ()
deleteDefinition name = do
  definitions <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = filter haveSameName definitions }
  where
    haveSameName :: Definition -> Bool
    haveSameName
      (GlobalDefinition (Function _ _ _ _ _ _ (Name oldName) _ _ _ _ _ _ _ _))
      | oldName == name = False
    haveSameName (TypeDefinition (Name oldName) _) | oldName == name = False
    haveSameName _ = True

define :: Type -> String -> [String] -> [BasicBlock] -> ModuleMaker ()
define funcType funcName argNames body = do
  assert (length (argTypes funcType) == length argNames)
         "Numbers of argument types and arguments don't match."

  deleteDefinition funcName
  addDefinition $ GlobalDefinition $ functionDefaults {
    name = Name funcName,
    parameters = ([Parameter argType (Name argName) []
                   | (argName, argType) <- zip argNames (argTypes funcType)],
                  False),
    returnType = retType funcType,
    basicBlocks = body
  }

declare :: Type -> String -> [String] -> ModuleMaker ()
declare funcType funcName argNames = do
  assert (length (argTypes funcType) == length argNames)
         "Numbers of argument types and arguments don't match."

  deleteDefinition funcName
  addDefinition $ GlobalDefinition $ functionDefaults {
    name = Name funcName,
    parameters = ([Parameter argType (Name argName) []
                   | (argName, argType) <- zip argNames (argTypes funcType)],
                  False),
    returnType = retType funcType,
    basicBlocks = []
  }

typeDef :: String -> Type -> ModuleMaker ()
typeDef name newType = do
  deleteDefinition name
  addDefinition $ TypeDefinition (Name name) (Just newType)

assert :: Bool -> String -> ModuleMaker ()
assert bool errorMessage = when (not bool) (fail errorMessage)
