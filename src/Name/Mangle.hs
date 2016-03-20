module Name.Mangle (
  unaryOpFuncName,
  binaryOpFuncName,
  closureName,
  closureEnvTypeName
) where



unaryOpFuncName :: String -> String
unaryOpFuncName = ("unary." ++)

binaryOpFuncName :: String -> String
binaryOpFuncName = ("binary." ++)

closureName :: String -> String
closureName funcName
  = langPrefix ++ funcName ++ ".closure"

closureEnvTypeName :: String -> String
closureEnvTypeName funcName
  = langPrefix ++ funcName ++ ".closure_env"

langPrefix :: String
langPrefix = "jack."
