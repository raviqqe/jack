{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen.Instruction where

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

import Codegen.FuncMaker
import Codegen.Type



-- Instruction and terminator

instruction :: Instruction -> FuncMaker Operand
instruction instr = do
  anonIndex <- fresh
  block <- getBlock
  let resultName = UnName anonIndex
  modifyBlock $ block { instructions = instructions block
                                       ++ [resultName := instr] }
  return $ local resultName
  where
    fresh :: FuncMaker Word
    fresh = do
      index <- gets anonInstrIndex
      modify $ \s -> s { anonInstrIndex = index + 1 }
      return (index + 1)

terminator :: Named Terminator -> FuncMaker (Named Terminator)
terminator arnold = do
  block <- getBlock
  modifyBlock $ block { blockTerminator = Just arnold }
  return arnold


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

uitofp :: Operand -> FuncMaker Operand
uitofp a = instruction $ UIToFP a double []

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

-- Control flow

br :: Name -> FuncMaker (Named Terminator)
br value = terminator $ Do $ Br value []

condbr :: Operand -> Name -> Name -> FuncMaker (Named Terminator)
condbr cond whenTrue whenFalse
  = terminator $ Do $ CondBr cond whenTrue whenFalse []

ret :: Operand -> FuncMaker (Named Terminator)
ret value = terminator $ Do $ Ret (Just value) []
