module Syntax where


data Stmt = Function Name [Expr] Expr
          | Extern Name [Expr]
          deriving (Eq, Ord, Show)

type Name = String

data Expr = Float Double
          | BinOp Op Expr Expr
          | Var Name
          | Call Name [Expr]
          deriving (Eq, Ord, Show)

data Op = Plus
        | Minus
        | Times
        | Divide
        deriving (Eq, Ord, Show)
