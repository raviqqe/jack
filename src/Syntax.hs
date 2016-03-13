module Syntax where


type Name = String

data Statement = STermDef Name [Name] Expr
               | SImport Name [Name]
               deriving (Eq, Ord, Show)

data Expr = ENum Double
          | EBool Bool
          | EVar Name
          | ECall Name [Expr]
          | EIf Expr Expr Expr
          deriving (Eq, Ord, Show)
