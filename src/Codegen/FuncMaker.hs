{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen.FuncMaker (
  FuncMaker,
  getNewAnonName,
  blocksInFunc,

  setBlock,
  getBlock,
  addBlock,
  appendInstruction,
  setTerminator,
) where

import Control.Monad.State
import Control.Applicative
import Data.Function
import Data.List
import Data.Word
import qualified Data.Map as Map
import LLVM.General.AST

import qualified Name.Supply as NS



-- FuncMaker state

data FuncMakerState =
  FuncMakerState {
    currentBlockName  :: Maybe Name,
      -- Name of the active block to append instructions to
    functionBlocks    :: Map.Map Name BlockState,
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

createBasicBlocks :: FuncMakerState -> [BasicBlock]
createBasicBlocks s
  = map toBasicBlock $ sortBlocks $ Map.toList (functionBlocks s)
  where
    sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
    sortBlocks = sortBy (compare `on` (blockIndex . snd))

    toBasicBlock :: (Name, BlockState) -> BasicBlock
    toBasicBlock (name, (BlockState _ instructions t))
      = BasicBlock name instructions (getTerminator t)
      where
        getTerminator (Just t) = t
        getTerminator Nothing = error $ "Block has no terminator: "
                                        ++ show name

emptyFuncMaker :: FuncMakerState
emptyFuncMaker =
  FuncMakerState {
    currentBlockName  = Nothing,
    functionBlocks    = Map.empty,
    anonInstrIndex    = 0,
    blockNames        = NS.empty
  }

execFuncMaker :: FuncMaker a -> FuncMakerState
execFuncMaker funcMaker
  = execState (runFuncMaker funcMakerWithEntry) emptyFuncMaker
  where
    funcMakerWithEntry = do
      setBlock =<< addBlock entryBlockName
      funcMaker

    entryBlockName :: String
    entryBlockName = "entry"

getNewAnonName :: FuncMaker Name
getNewAnonName = do
  anonIndex <- nextAnonInstrIndex
  return (UnName anonIndex)
  where
    nextAnonInstrIndex :: FuncMaker Word
    nextAnonInstrIndex = do
      index <- gets anonInstrIndex
      modify $ \s -> s { anonInstrIndex = index + 1 }
      return (index + 1)

blocksInFunc :: FuncMaker a -> [BasicBlock]
blocksInFunc = createBasicBlocks . execFuncMaker


-- Block stack

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

setBlock :: Name -> FuncMaker ()
setBlock name = modify $ \s -> s { currentBlockName = Just name }

getBlockState :: FuncMaker BlockState
getBlockState = do
  name <- getBlock
  blocks <- gets functionBlocks
  case Map.lookup name blocks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show name

getBlock :: FuncMaker Name
getBlock = do
  maybeName <- gets currentBlockName
  return (case maybeName of
    Just name -> name
    Nothing -> error "Current block is not set yet.")

modifyBlock :: BlockState -> FuncMaker ()
modifyBlock newBlock = do
  name <- getBlock
  modify $ \s -> s { functionBlocks = Map.insert name newBlock
                                                 (functionBlocks s) }

appendInstruction :: Named Instruction -> FuncMaker ()
appendInstruction instruction = do
  block <- getBlockState
  modifyBlock $ block { instructions = instructions block ++ [instruction] }

setTerminator :: Named Terminator -> FuncMaker ()
setTerminator arnold = do
  block <- getBlockState
  modifyBlock $ block { blockTerminator = Just arnold }
