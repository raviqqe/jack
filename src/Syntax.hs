module Syntax where


type Name = String

data Stmt = Function Name [Name] Expr
          | Extern Name [Name]
          deriving (Eq, Ord, Show)

data Expr = Float Double
          | Var Name
          | Call Name [Expr]
          | BinaryOp Name Expr Expr
          | UnaryOp Name Expr
          deriving (Eq, Ord, Show)
