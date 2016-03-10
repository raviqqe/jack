module Syntax where



type Name = String

data Assoc = LeftAssoc | RightAssoc

data OperatorKind = Prefix | Infix

data Stmt = Function Name [Name] Expr
          | Extern Name [Name]
          | OperatorDeclaration Name OperatorKind Assoc
          | OperatorDefinition Name [Name] Expr
          deriving (Eq, Ord, Show)

data Expr = Float Double
          | Boolean Bool
          | Var Name
          | Call Name [Expr]
          | BinaryOp Name Expr Expr
          | UnaryOp Name Expr
          | If Expr Expr Expr
          deriving (Eq, Ord, Show)
