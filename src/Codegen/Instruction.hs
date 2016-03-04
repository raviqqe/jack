module Codegen.Instruction (
  fadd,
  fsub,
  fmul,
  fdiv,
  fcmp,
  uitofp,
  constant,

  call,
  alloca,
  store,
  load,

  br,
  condbr,
  ret,
) where

import LLVM.General.AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Codegen.FuncMaker
import Codegen.Type



-- Instruction and terminator

instruction :: Instruction -> FuncMaker Operand
instruction instr = do
  resultName <- getNewAnonName
  appendInstruction (resultName := instr)
  return $ localRef resultName

noOpInstruction :: Instruction -> FuncMaker ()
noOpInstruction instr = appendInstruction (Do instr)

terminator :: Named Terminator -> FuncMaker (Named Terminator)
terminator arnold = do
  setTerminator arnold
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

store :: Operand -> Operand -> FuncMaker ()
store pointer value = noOpInstruction $ Store False pointer value Nothing 0 []

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
