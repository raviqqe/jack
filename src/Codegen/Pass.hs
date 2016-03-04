module Codegen.Pass (
  passes
) where

import LLVM.General.PassManager



passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }
