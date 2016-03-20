module Name.Mangle (
  unaryOpFuncName,
  binaryOpFuncName,
  closureEnvName
) where



unaryOpFuncName :: String -> String
unaryOpFuncName = ("unary." ++)

binaryOpFuncName :: String -> String
binaryOpFuncName = ("binary." ++)

closureEnvName :: String -> String
closureEnvName funcNameWithTypes
  = langPrefix ++ funcNameWithTypes ++ ".closure_env"

langPrefix :: String
langPrefix = "jack."
