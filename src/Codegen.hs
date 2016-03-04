{-# LANGUAGE OverloadedStrings #-}

module Codegen (
  codegen,
  emptyModule
) where

import Control.Monad.Except
import Data.String
import qualified Data.Map as Map
import LLVM.General.Analysis
import LLVM.General.Module
import LLVM.General.Context
import LLVM.General.PassManager
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Codegen.ModuleMaker
import Codegen.FuncMaker
import Codegen.Instruction
import Codegen.Pass
import Codegen.Type
import qualified Syntax as S



toSignatures :: [String] -> [(AST.Type, AST.Name)]
toSignatures = map (\name -> (double, AST.Name name))

codegenToplevel :: Either S.Expr S.Stmt -> ModuleMaker ()
codegenToplevel (Right (S.Function name argNames body)) = do
  define double name args blocks
  where
    args = toSignatures argNames
    blocks = blocksInFunc $ do
      forM_ argNames $ \argName -> do
        var <- alloca double
        store var (localRef double (AST.Name argName))
        setSymbol argName var
      ret =<< codegenExpr body

codegenToplevel (Right (S.Extern name argNames)) = declare double name args
  where
    args = toSignatures argNames

codegenToplevel (Left expression) = define double "main" [] blocks
  where
    blocks = blocksInFunc $ ret =<< codegenExpr expression


-- Operations

lt :: AST.Operand -> AST.Operand -> FuncMaker AST.Operand
lt a b = uitofp =<< fcmp FP.ULT a b

binops :: Map.Map String (AST.Operand -> AST.Operand -> FuncMaker AST.Operand)
binops = Map.fromList [
    ("+", fadd),
    ("-", fsub),
    ("*", fmul),
    ("/", fdiv),
    ("<", lt)
  ]

codegenExpr :: S.Expr -> FuncMaker AST.Operand
codegenExpr (S.UnaryOp operatorName arg) = do
  codegenExpr $ S.Call ("unary" ++ operatorName) [arg]
codegenExpr (S.BinaryOp "=" (S.Var varName) expression) = do
  var <- referToSymbol varName
  value <- codegenExpr expression
  store var value
  return value
codegenExpr (S.BinaryOp operator a b) = do
  case Map.lookup operator binops of
    Just instruction -> do
      ca <- codegenExpr a
      cb <- codegenExpr b
      instruction ca cb
    Nothing -> error "No such operator"
codegenExpr (S.Var varName) = load =<< referToSymbol varName
codegenExpr (S.Float num) = (return . constant . C.Float . F.Double) num
codegenExpr (S.Call functionName args) = do
  call (globalRef double (AST.Name functionName)) =<< mapM codegenExpr args

-- Compilation

liftExceptT :: ExceptT String IO a -> IO a
liftExceptT = runExceptT >=> either fail return

codegen :: AST.Module -> [Either S.Expr S.Stmt] -> IO AST.Module
codegen astMod toplevels = withContext $ \context -> do
  let newAstMod = runModuleMaker astMod $ mapM codegenToplevel toplevels
  liftExceptT $ withModuleFromAST context newAstMod $ \mod -> do
    withPassManager passes $ \passManager -> do
      liftExceptT $ verify mod
      ok <- runPassManager passManager mod
      unless ok $ fail "Pass manager failed."
      putStrLn =<< moduleLLVMAssembly mod
      return =<< moduleAST mod

-- Name

instance IsString AST.Name where
  fromString = AST.Name . fromString
