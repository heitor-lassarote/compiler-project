-- | The definition of the evaluation syntax
-- it's almost like an interpreter so probably
-- in the future we will try to make something with LLVM
-- The "Value" type represents the neutral form
-- of an computation.
module Language.Core.Syntax (
  Value(..),
  Stuck(..),
  Hole(..),
  Spine,
  Meta,
  Lvl,
  Idx,
  pattern VVar
) where

import Language.Core.PreSyntax

newtype Idx = Idx Int deriving newtype Num
newtype Lvl = Lvl Int deriving newtype Num

type Spine = [Value]

type Meta = IORef Hole

data Hole
  = Empty Lvl
  | Filled Value

-- | An application that is stuck by
-- one of the reasons below
data Stuck
  = Flex Meta
  | Rigid Lvl
  | Top Text

data Value
  = VConst Lit
  | VStuck Stuck Spine
  | VBinder Binder (Value -> Value)

pattern VVar :: Lvl -> Value
pattern VVar i = VStuck (Rigid i) []