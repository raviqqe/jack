{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen.Type (
  double
) where

import LLVM.General.AST



double :: Type -- IEEE 754
double = FloatingPointType 64 IEEE
