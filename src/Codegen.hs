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


-- Codegen state

type SymbolTable = [(String, Operand)]

data CodegenState =
  CodegenState {
    currentBlock    :: Name, -- Name of the active block to append to
    functionBlocks  :: Map.Map Name BlockState, -- Blocks for a function
    symbolTable     :: SymbolTable, -- symbol table of function scope
    anonInstrIndex  :: Word, -- Count of unnamed instructions
    names           :: Names -- Name Supply
  } deriving Show

data BlockState =
  BlockState {
    blockIndex      :: Int, -- Block index
    instructions    :: [Named Instruction], -- Stack of instructions
    blockTerminator :: Maybe (Named Terminator) -- Block terminator
  } deriving Show


-- Codegen operation

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (blockIndex . snd))

createBlocks :: CodegenState -> [BasicBlock]
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

emptyCodegen :: CodegenState
emptyCodegen =
  CodegenState {
    currentBlock    = Name entryBlockName,
    functionBlocks  = Map.empty,
    symbolTable     = [],
    anonInstrIndex  = 0,
    names           = Map.empty
  }

execCodegen :: Codegen a -> CodegenState
execCodegen codegen = execState (runCodegen codegen) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets anonInstrIndex
  modify $ \state -> state { anonInstrIndex = i + 1 }
  return (i + 1)

instruction :: Instruction -> Codegen (Operand)
instruction i = do
  name <- fresh
  block <- current
  let reference = UnName name
      is = instructions block
  modifyBlock $ block { instructions = is ++ [reference := i] }
  return $ local reference

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator arnold = do
  block <- current
  modifyBlock $ block { blockTerminator = Just arnold }
  return arnold


-- Block stack

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
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


setBlock :: Name -> Codegen Name
setBlock name = do
  modify $ \s -> s { currentBlock = name }
  return name

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock newBlock = do
  name <- gets currentBlock
  modify $ \s -> s { functionBlocks = Map.insert name newBlock
                                                 (functionBlocks s) }

current :: Codegen BlockState
current = do
  name <- gets currentBlock
  blocks <- gets functionBlocks
  case Map.lookup name blocks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show name


-- Symbol Table

assign :: String -> Operand -> Codegen ()
assign variableName value = do
  symbols <- gets symbolTable
  modify $ \s -> s { symbolTable = [(variableName, value)] ++ symbols }

getVar :: String -> Codegen Operand
getVar variableName = do
  symbols <- gets symbolTable
  case lookup variableName symbols of
    Just x -> return x
    Nothing -> error $ "Local variable not in scope: " ++ variableName


-- References

local :: Name -> Operand
local = LocalReference double

global :: Name -> Operand
global = ConstantOperand . C.GlobalReference double

-- Arithmetic and constants

fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instruction $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instruction $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instruction $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instruction $ FDiv NoFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instruction $ FCmp cond a b []

constant :: C.Constant -> Operand
constant = ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp typ a = instruction $ UIToFP a typ []

-- Effects

call :: Operand -> [Operand] -> Codegen Operand
call function args
  = instruction $ Call Nothing CC.C [] (Right function) (toArgs args) [] []
  where
    toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
    toArgs = map (\x -> (x, []))

alloca :: Type -> Codegen Operand
alloca typ = instruction $ Alloca typ Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store pointer value = instruction $ Store False pointer value Nothing 0 []

load :: Operand -> Codegen Operand
load pointer = instruction $ Load False pointer Nothing 0 []

---- Control flow

br :: Name -> Codegen (Named Terminator)
br value = terminator $ Do $ Br value []

condbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
condbr cond whenTrue whenFalse
  = terminator $ Do $ CondBr cond whenTrue whenFalse []

ret :: Operand -> Codegen (Named Terminator)
ret value = terminator $ Do $ Ret (Just value) []
