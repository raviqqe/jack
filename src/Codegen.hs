module Codegen (
  Module,
  codegen,
  initialModule,
  assemblyFromModule
) where

import Control.Monad.Except
import LLVM.General.Analysis
import LLVM.General.Context
import LLVM.General.PassManager
import qualified LLVM.General.Module as M
import LLVM.General.AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Codegen.ModuleMaker
import Codegen.FuncMaker
import Codegen.InitialModule
import Codegen.Instruction
import Codegen.Pass
import Codegen.Type
import Constant
import Util
import qualified Syntax as S



codegen :: Module -> [Either S.Expr S.Statement] -> IO Module
codegen mod toplevels = withContext $ \context -> do
  let newMod = runModuleMaker mod $ mapM codegenToplevel toplevels
  liftExceptT $ M.withModuleFromAST context newMod $ \modObj -> do
    withPassManager passes $ \passManager -> do
      liftExceptT $ verify modObj
      _ <- runPassManager passManager modObj
      liftExceptT $ verify modObj
      M.moduleAST modObj

codegenToplevel :: Either S.Expr S.Statement -> ModuleMaker ()
codegenToplevel (Right (S.STermDef name argNames body)) = do
  define double name args blocks
  where
    args = toSignatures argNames
    blocks = blocksInFunc $ do
      forM_ argNames $ \argName -> do
        pointer <- alloca double
        store (localRef (Name argName)) pointer
        setSymbol argName pointer
      ret =<< codegenExpr body
codegenToplevel (Right (S.SImport name argNames)) = declare double name args
  where
    args = toSignatures argNames
codegenToplevel (Left expression)
  = define double toplevelExprFuncName [] blocks
  where
    blocks = blocksInFunc $ ret =<< codegenExpr expression

codegenExpr :: S.Expr -> FuncMaker Operand
codegenExpr (S.EVar varName) = load =<< referToSymbol varName
codegenExpr (S.ENum num) = (return . constant . C.Float . F.Double) num
codegenExpr (S.ECall functionName args) = do
  call (globalRef (Name functionName)) =<< mapM codegenExpr args
codegenExpr (S.EIf cond exprIfTrue exprIfFalse) = do
  thenBlock <- addBlock "if.then"
  elseBlock <- addBlock "if.else"
  exitIfBlock <- addBlock "if.exit"

  cond <- fcmp FP.ONE false =<< codegenExpr cond
  condbr cond thenBlock elseBlock

  setBlock thenBlock
  valueIfTrue <- codegenExpr exprIfTrue
  br exitIfBlock
  newThenBlock <- getBlock

  setBlock elseBlock
  valueIfFalse <- codegenExpr exprIfFalse
  br exitIfBlock
  newElseBlock <- getBlock

  setBlock exitIfBlock
  phi double [(valueIfTrue, newThenBlock), (valueIfFalse, newElseBlock)]
codegenExpr (S.EBool True) = return true
codegenExpr (S.EBool False) = return false

true :: Operand
true  = constant $ C.Float (F.Double 1.0)
false :: Operand
false = constant $ C.Float (F.Double 0.0)

toSignatures :: [String] -> [(Type, Name)]
toSignatures = map (\name -> (double, Name name))

assemblyFromModule :: Module -> IO String
assemblyFromModule mod = do
  withContext $ \context -> do
    liftExceptT $ M.withModuleFromAST context mod $ \modObj -> do
      M.moduleLLVMAssembly modObj
