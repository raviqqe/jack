{-# LANGUAGE OverloadedStrings #-}

module Emit (
  codegen
) where

import Control.Applicative
import Control.Monad.Except
import Data.Int
import Data.Word
import qualified Data.Map as Map
import LLVM.General.Module
import LLVM.General.Context
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Codegen
import qualified Syntax as S



toSignatures :: [String] -> [(AST.Type, AST.Name)]
toSignatures = map (\name -> (double, AST.Name name))

codegenToplevel :: Either S.Expr S.Stmt -> ModuleMaker ()
codegenToplevel (Right (S.Function name argNames body)) = do
  define double name args blocks
  where
    args = toSignatures argNames
    blocks = createBlocks $ execCodegen $ do
      setBlock =<< addBlock entryBlockName
      forM argNames $ \argName -> do
        var <- alloca double
        store var (local (AST.Name argName))
        assign argName var
      ret =<< codegenExpr body

codegenToplevel (Right (S.Extern name argNames)) = external double name args
  where
    args = toSignatures argNames

codegenToplevel (Left expression) = define double "main" [] blocks
  where
    blocks = createBlocks $ execCodegen $ do
      setBlock =<< addBlock entryBlockName
      ret =<< codegenExpr expression


-- Operations

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = uitofp double =<< fcmp FP.ULT a b

binops = Map.fromList [
    ("+", fadd),
    ("-", fsub),
    ("*", fmul),
    ("/", fdiv),
    ("<", lt)
  ]

codegenExpr :: S.Expr -> Codegen AST.Operand
codegenExpr (S.UnaryOp operatorName arg) = do
  codegenExpr $ S.Call ("unary" ++ operatorName) [arg]
codegenExpr (S.BinaryOp "=" (S.Var varName) value) = do
  var <- getVar varName
  convertedValue <- codegenExpr value
  store var convertedValue
  return convertedValue
codegenExpr (S.BinaryOp operator a b) = do
  case Map.lookup operator binops of
    Just f -> do
      ca <- codegenExpr a
      cb <- codegenExpr b
      f ca cb
    Nothing -> error "No such operator"
codegenExpr (S.Var varName) = load =<< getVar varName
codegenExpr (S.Float n) = return $ constant $ C.Float (F.Double n)
codegenExpr (S.Call function args) = do
  largs <- mapM codegenExpr args
  call (externf (AST.Name function)) largs

-- Compilation

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [Either S.Expr S.Stmt] -> IO AST.Module
codegen astMod toplevels = withContext $ \context ->
  liftError $ withModuleFromAST context astMod' $ \mod -> do
    assembly <- moduleLLVMAssembly mod
    putStrLn assembly
    return astMod'
  where
    astMod' = runModuleMaker astMod $ mapM codegenToplevel toplevels
