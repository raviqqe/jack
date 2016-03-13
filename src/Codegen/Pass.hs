module Codegen.Pass (
  passes
) where

import LLVM.General.PassManager
import LLVM.General.Transforms



--passes :: PassSetSpec
--passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

-- Don't delete extra declarations optimizing modules
passes :: PassSetSpec
passes = defaultPassSetSpec {
    transforms = [
      --PromoteMemoryToRegister
    ]
  }
