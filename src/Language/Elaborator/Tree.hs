module Language.Elaborator.Tree
  ( Name
  , Expr (..)
  ) where

type Name = Text

data Expr
  = Typ
  | Var Name
  | Lam Name Expr
  | App Expr Expr
  | Pi Name Expr Expr
  | Ann Expr Expr
  | Let Name Expr Expr Expr
  | Hol
  deriving stock (Eq, Show)
