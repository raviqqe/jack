{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Control.Monad.State
import Control.Applicative
import Data.Function
import Data.String
import Data.List
import Data.Word
import qualified Data.Map as Map
import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.FloatingPointPredicate as FP



-- Module level

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

external :: Type -> String -> [(Type, Name)] -> ModuleMaker ()
external retType label args = addDefinition $
  GlobalDefinition $ functionDefaults {
    name = Name label,
    parameters = ([Parameter argType name [] | (argType, name) <- args],
                  False),
    returnType = retType,
    basicBlocks = []
  }


-- Types

double :: Type -- IEEE 754
double = FloatingPointType 64 IEEE


-- Names

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName name names =
  case Map.lookup name names of
    Nothing -> (name, Map.insert name 0 names)
    Just index -> (name ++ show (index + 1), Map.insert name (index + 1) names)

instance IsString Name where
  fromString = Name . fromString


-- FuncMaker state

type SymbolTable = [(String, Operand)]

data FuncMakerState =
  FuncMakerState {
    currentBlockName  :: Name, -- Name of the active block to append to
    functionBlocks    :: Map.Map Name BlockState, -- Blocks for a function
    symbolTable       :: SymbolTable, -- symbol table of function scope
    anonInstrIndex    :: Word, -- Count of unnamed instructions
    names             :: Names -- Name Supply
  } deriving Show

data BlockState =
  BlockState {
    blockIndex      :: Int, -- Block index
    instructions    :: [Named Instruction], -- Stack of instructions
    blockTerminator :: Maybe (Named Terminator) -- Block terminator
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
    symbolTable       = [],
    anonInstrIndex    = 0,
    names             = Map.empty
  }

execFuncMaker :: FuncMaker a -> FuncMakerState
execFuncMaker codegen = execState (runFuncMaker codegen) emptyFuncMaker

fresh :: FuncMaker Word
fresh = do
  index <- gets anonInstrIndex
  modify $ \s -> s { anonInstrIndex = index + 1 }
  return (index + 1)

instruction :: Instruction -> FuncMaker Operand
instruction instr = do
  anonIndex <- fresh
  block <- getBlock
  let resultName = UnName anonIndex
  modifyBlock $ block { instructions = instructions block
                                       ++ [resultName := instr] }
  return $ local resultName

terminator :: Named Terminator -> FuncMaker (Named Terminator)
terminator arnold = do
  block <- getBlock
  modifyBlock $ block { blockTerminator = Just arnold }
  return arnold


-- Block stack

entry :: FuncMaker Name
entry = gets currentBlockName

addBlock :: String -> FuncMaker Name
addBlock name = do
  blocks <- gets functionBlocks
  lessNames <- gets names
  let newBlock = emptyBlock (Map.size blocks)
      (qualifiedName, moreNames) = uniqueName name lessNames
  modify $ \s -> s { functionBlocks = Map.insert (Name qualifiedName) newBlock
                                                 blocks,
                     names = moreNames }
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
  modify $ \s -> s { symbolTable = [(varName, value)] ++ symbols }

getVar :: String -> FuncMaker Operand
getVar varName = do
  symbols <- gets symbolTable
  case lookup varName symbols of
    Just x -> return x
    Nothing -> error $ "Local variable not in scope: " ++ varName


-- References

local :: Name -> Operand
local = LocalReference double

global :: Name -> Operand
global = ConstantOperand . C.GlobalReference double

-- Arithmetic and constants

fadd :: Operand -> Operand -> FuncMaker Operand
fadd a b = instruction $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> FuncMaker Operand
fsub a b = instruction $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> FuncMaker Operand
fmul a b = instruction $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> FuncMaker Operand
fdiv a b = instruction $ FDiv NoFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> FuncMaker Operand
fcmp cond a b = instruction $ FCmp cond a b []

constant :: C.Constant -> Operand
constant = ConstantOperand

uitofp :: Type -> Operand -> FuncMaker Operand
uitofp typ a = instruction $ UIToFP a typ []

-- Effects

call :: Operand -> [Operand] -> FuncMaker Operand
call function args
  = instruction $ Call Nothing CC.C [] (Right function) (toArgs args) [] []
  where
    toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
    toArgs = map (\x -> (x, []))

alloca :: Type -> FuncMaker Operand
alloca typ = instruction $ Alloca typ Nothing 0 []

store :: Operand -> Operand -> FuncMaker Operand
store pointer value = instruction $ Store False pointer value Nothing 0 []

load :: Operand -> FuncMaker Operand
load pointer = instruction $ Load False pointer Nothing 0 []

---- Control flow

br :: Name -> FuncMaker (Named Terminator)
br value = terminator $ Do $ Br value []

condbr :: Operand -> Name -> Name -> FuncMaker (Named Terminator)
condbr cond whenTrue whenFalse
  = terminator $ Do $ CondBr cond whenTrue whenFalse []

ret :: Operand -> FuncMaker (Named Terminator)
ret value = terminator $ Do $ Ret (Just value) []
