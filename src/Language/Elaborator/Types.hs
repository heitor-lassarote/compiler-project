module Language.Elaborator.Types
  ( module Language.Elaborator.Types
  ) where

import Unsafe qualified

import Language.Elaborator.Tree (Name)

newtype Lvl = Lvl{getLvl :: Int}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)

newtype Idx = Idx{getIdx :: Int}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num)

lvlToIdx :: Lvl -> Idx -> Idx
lvlToIdx (Lvl l) (Idx x) = Idx (l - x - 1)

idxToLvl :: Idx -> Lvl -> Lvl
idxToLvl (Idx x) (Lvl l) = Lvl (l + x + 1)

data Term
  = Type
  | Var Idx
  | Free Name
  | Lam Name Term
  | Let Name Term Term Term
  | App Term Term
  | Pi Name Term Term
  | Ann Term Term
  | Hol (IORef Hole)
  deriving stock Eq

ppTerm :: [Name] -> Term -> Text
ppTerm ctx = \case
  Type -> "*"
  Var (Idx idx) -> ctx Unsafe.!! idx
  Free name -> name
  Lam name body -> "(\\" <> name <> " => " <> ppTerm (name : ctx) body <> ")"
  p@App{} -> "(" <> unwords (accumApp [] p) <> ")"
  Let name ty val body -> "(let " <> name <> " : " <> ppTerm ctx ty <> " = " <> ppTerm ctx val <> " in " <> ppTerm ctx body <> ")"
  Pi var typ body
    | var == "_" -> "(" <> ppTerm ctx typ <> " -> " <> ppTerm ("_":ctx) body <> ")"
    | otherwise  -> "((" <> var <> ": " <> ppTerm ctx typ <> ") -> " <> ppTerm (var: ctx) body <> ")"
  Ann var typ -> "(" <> ppTerm ctx var <> " : " <> ppTerm ctx typ <> ")"
  Hol _ -> "?"
  where
    accumApp acc (App f@App{} y) = accumApp (ppTerm ctx y : acc) f
    accumApp acc (App f x) = ppTerm ctx f : ppTerm ctx x : acc
    accumApp acc x = ppTerm ctx x : acc

data Hole
  = Empty Lvl
  | Full Value

data Value
  = VType
  -- | Represents an application that is stuck because we've used a bounded
  -- variable.
  | VRigid Idx [Value]
  -- | Represents an application that is stuck because we haven't yet decided
  -- what is contained inside the 'IORef' field.
  | VFlex (IORef Hole) [Value]
  -- | Refers to an application that is at top-level. Allows optimizing by
  -- comparing its arguments before running the function.
  | VTop Name [Value]
  | VLam Name (Value -> Value)
  | VPi Name Value (Value -> Value)

vVar :: Lvl -> Value
vVar (Lvl l) = VRigid (Idx l) []
