{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen.FuncMaker where

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
import Codegen.Type



-- FuncMaker state

type SymbolTable = Map.Map String Operand

data FuncMakerState =
  FuncMakerState {
    currentBlockName  :: Name, -- Name of the active block to append instrs to
    functionBlocks    :: Map.Map Name BlockState,
    symbolTable       :: SymbolTable, -- Symbol table of function scope
    anonInstrIndex    :: Word,
    blockNames        :: NS.NameSupply
  } deriving Show

data BlockState =
  BlockState {
    blockIndex      :: Int,
    instructions    :: [Named Instruction],
    blockTerminator :: Maybe (Named Terminator)
  } deriving Show


-- FuncMaker operation

newtype FuncMaker a = FuncMaker { runFuncMaker :: State FuncMakerState a }
  deriving (Functor, Applicative, Monad, MonadState FuncMakerState)

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (blockIndex . snd))

createBlocks :: FuncMakerState -> [BasicBlock]
createBlocks s = map toBasicBlock $ sortBlocks $ Map.toList (functionBlocks s)
  where
    toBasicBlock :: (Name, BlockState) -> BasicBlock
    toBasicBlock (name, (BlockState _ instructions t))
      = BasicBlock name instructions (getTerminator t)
      where
        getTerminator (Just t) = t
        getTerminator Nothing = error $ "Block has no terminator: "
                                        ++ show name

entryBlockName :: String
entryBlockName = "entry"

emptyFuncMaker :: FuncMakerState
emptyFuncMaker =
  FuncMakerState {
    currentBlockName  = Name entryBlockName,
    functionBlocks    = Map.empty,
    symbolTable       = Map.empty,
    anonInstrIndex    = 0,
    blockNames        = NS.insert entryBlockName NS.empty
  }

execFuncMaker :: FuncMaker a -> FuncMakerState
execFuncMaker codegen = execState (runFuncMaker codegen) emptyFuncMaker

-- Block stack

entry :: FuncMaker Name
entry = gets currentBlockName

addBlock :: String -> FuncMaker Name
addBlock name = do
  blocks <- gets functionBlocks
  lessNames <- gets blockNames
  let newBlock = emptyBlock (Map.size blocks)
      (qualifiedName, moreNames) = NS.uniqueName name lessNames
  modify $ \s -> s { functionBlocks = Map.insert (Name qualifiedName) newBlock
                                                 blocks,
                     blockNames = moreNames }
  return $ Name qualifiedName
  where
    emptyBlock index = BlockState index [] Nothing

setBlock :: Name -> FuncMaker Name
setBlock name = do
  modify $ \s -> s { currentBlockName = name }
  return name

getBlock :: FuncMaker BlockState
getBlock = do
  name <- gets currentBlockName
  blocks <- gets functionBlocks
  case Map.lookup name blocks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show name

modifyBlock :: BlockState -> FuncMaker ()
modifyBlock newBlock = do
  name <- gets currentBlockName
  modify $ \s -> s { functionBlocks = Map.insert name newBlock
                                                 (functionBlocks s) }


-- Symbol Table

assign :: String -> Operand -> FuncMaker ()
assign varName value = do
  symbols <- gets symbolTable
  modify $ \s -> s { symbolTable = Map.insert varName value symbols }

getVar :: String -> FuncMaker Operand
getVar varName = do
  symbols <- gets symbolTable
  case Map.lookup varName symbols of
    Just x -> return x
    Nothing -> error $ "Local variable not in scope: " ++ varName


-- References

local :: Name -> Operand
local = LocalReference double

global :: Name -> Operand
global = ConstantOperand . C.GlobalReference double
