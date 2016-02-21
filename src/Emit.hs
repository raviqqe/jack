{-# LANGUAGE OverloadedStrings #-}

module Emit where

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

codegenTop :: Either S.Expr S.Stmt -> LLVM ()
codegenTop (Right (S.Function name args body)) = do
  define double name args' blocks
  where
    args' = toSignatures args
    blocks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \arg -> do
        var <- alloca double
        store var (local (AST.Name arg))
        assign arg var
      ret =<< cgen body

codegenTop (Right (S.Extern name args)) = do
  external double name args'
  where
    args' = toSignatures args

codegenTop (Left expression) = do
  define double "main" [] blocks
  where
    blocks = createBlocks $ execCodegen $ do
      setBlock =<< addBlock entryBlockName
      ret =<< cgen expression


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

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.UnaryOp operatorName arg) = do
  cgen $ S.Call ("unary" ++ operatorName) [arg]
cgen (S.BinaryOp "=" (S.Var varName) value) = do
  var <- getVar varName
  convertedValue <- cgen value
  store var convertedValue
  return convertedValue
cgen (S.BinaryOp operator a b) = do
  case Map.lookup operator binops of
    Just f -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
cgen (S.Var varName) = load =<< getVar varName
cgen (S.Float n) = return $ constant $ C.Float (F.Double n)
cgen (S.Call function args) = do
  largs <- mapM cgen args
  call (externf (AST.Name function)) largs

-- Compilation

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [Either S.Expr S.Stmt] -> IO AST.Module
codegen astMod functions = withContext $ \context ->
  liftError $ withModuleFromAST context astMod' $ \mod -> do
    assembly <- moduleLLVMAssembly mod
    putStrLn assembly
    return astMod'
  where
    astMod' = runLLVM astMod $ mapM codegenTop functions
