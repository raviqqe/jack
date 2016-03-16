module Name.Mangle (
  unaryOpFuncName,
  binaryOpFuncName
) where



unaryOpFuncName :: String -> String
unaryOpFuncName = ("unary." ++)

binaryOpFuncName :: String -> String
binaryOpFuncName = ("binary." ++)
