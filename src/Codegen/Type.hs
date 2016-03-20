module Codegen.Type (
  void,
  byte,
  int,
  float,

  ptr,
  struct,
  array,
  func,

  argTypes,
  retType
) where

import qualified LLVM.General.AST.Type as T



void :: T.Type
void = T.void

byte :: T.Type
byte = T.i8

int :: T.Type
int = T.i64

float :: T.Type
float = T.double

ptr :: T.Type -> T.Type
ptr = T.ptr

struct :: [T.Type] -> T.Type
struct types = T.StructureType False types

array :: Int -> T.Type -> T.Type
array numOfElems typ = T.ArrayType (toEnum numOfElems) typ

func :: T.Type -> [T.Type] -> T.Type
func resultType argTypes = T.FunctionType resultType argTypes False

retType :: T.Type -> T.Type
retType = T.resultType

argTypes :: T.Type -> [T.Type]
argTypes = T.argumentTypes
