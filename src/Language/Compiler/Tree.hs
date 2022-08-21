module Language.Compiler.Tree
  ( Name
  , Expr (..)
  ) where

type Name = Text

data Expr
  = Type
  | Var Name
  | Lam Name Expr
  | App Expr Expr
  | Pi Name Expr Expr
  | Ann Expr Expr
  deriving stock (Eq, Show)
