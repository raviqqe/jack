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
  malloc,
  free,

  br,
  condbr,
  ret,
  phi,

  localRef,
  globalRef
) where

import qualified Control.Monad as M
import LLVM.General.AST
import qualified LLVM.General.AST.AddrSpace as AS
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

terminator :: Named Terminator -> FuncMaker ()
terminator arnold = setTerminator arnold


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
uitofp unsignedInt = instruction $ UIToFP unsignedInt double []


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
store value pointer = noOpInstruction $ Store False pointer value Nothing 0 []

load :: Operand -> FuncMaker Operand
load pointer = instruction $ Load False pointer Nothing 0 []

malloc :: Type -> FuncMaker Operand
malloc typ = do
  size <- sizeof typ
  call (globalRef (Name "malloc")) [size]

free :: Operand -> FuncMaker ()
free pointer = M.void $ call (globalRef (Name "free")) [pointer]

sizeof :: Type -> FuncMaker Operand
sizeof typ = do
  pointer <- getelementptr nullPointer [one]
  ptrtoint pointer
  where
    nullPointer = constant $ C.Null $ PointerType typ (AS.AddrSpace 0)
    one = constant (C.Int 32 1)

getelementptr :: Operand -> [Operand] -> FuncMaker Operand
getelementptr basePointer indices
  = instruction $ GetElementPtr False basePointer indices []


-- Conversion

ptrtoint :: Operand -> FuncMaker Operand
ptrtoint pointer = instruction $ PtrToInt pointer i64 []


-- Control flow

br :: Name -> FuncMaker ()
br value = terminator $ Do $ Br value []

condbr :: Operand -> Name -> Name -> FuncMaker ()
condbr cond whenTrue whenFalse
  = terminator $ Do $ CondBr cond whenTrue whenFalse []

ret :: Operand -> FuncMaker ()
ret value = terminator $ Do $ Ret (Just value) []

phi :: Type -> [(Operand, Name)] -> FuncMaker Operand
phi typ valueBlockPairs = instruction $ Phi typ valueBlockPairs []


-- References

-- HACK
-- `Type` fields of `LocalReference` and `GlobalReference` are not used
-- because they are not used in LLVM.General library and inconvenient
-- for us to use as there is no common way to extract types from `Operand`
-- data including `Constant`.

localRef :: Name -> Operand
localRef = LocalReference void -- HACK

globalRef :: Name -> Operand
globalRef = ConstantOperand . C.GlobalReference void -- HACK
