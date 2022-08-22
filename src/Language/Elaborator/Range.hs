module Language.Elaborator.Range
  ( Pos (..)
  , Range (..)
  , (<->)
  ) where

data Pos = Pos
  { offset, line, column :: Int
  } deriving stock (Eq, Ord, Show)

data Range = Range
  { start, stop :: Pos
  } deriving stock (Eq, Ord, Show)

-- | Performs the union of two ranges by creating a new range starting at the
-- start position of the first range, and stopping at the stop position of the
-- second range.
-- Invariant: The LHS range starts before the RHS range.
(<->) :: Range -> Range -> Range
Range a1 _ <-> Range _ b2 = Range a1 b2
