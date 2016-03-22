module Codegen.Constant (
  struct,
  ptrtoptr,
  int,
  int32,
  float,
  nullptr,
  globalRef
) where

import LLVM.General.AST
import LLVM.General.AST.Constant as C
import LLVM.General.AST.Float

import qualified Codegen.Type as T



ptrtoptr :: Constant -> Type -> Constant
ptrtoptr pointer destPointerType
  = C.IntToPtr (C.PtrToInt pointer T.int) destPointerType

struct :: String -> [Constant] -> Constant
struct name elems = Struct (Just $ Name name) False elems

int :: Integer -> Constant
int num = C.Int 64 num

int32 :: Integer -> Constant
int32 num = C.Int 32 num

float :: Double -> Constant
float = C.Float . Double

nullptr :: Type -> Constant
nullptr = Null

globalRef :: Name -> Constant
globalRef = C.GlobalReference T.void -- HACK: ignore Type argument
