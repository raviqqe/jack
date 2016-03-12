module Syntax where


type Name = String

data Stmt = Function Name [Name] Expr
          | Extern Name [Name]
          deriving (Eq, Ord, Show)

data Expr = Float Double
          | Boolean Bool
          | Var Name
          | Call Name [Expr]
          | If Expr Expr Expr
          deriving (Eq, Ord, Show)
