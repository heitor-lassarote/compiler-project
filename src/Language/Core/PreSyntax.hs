-- | The definition of a quoted syntax
-- for the language.
module Language.Core.PreSyntax where

-- | Things that create values.
data Binder
  = Lam Text
  | Let Text Term Term
  | Pi  Text Term

-- | Constant expressins.
data Lit
  = Str Text
  | Int Int
  | Typ

data VarKind
  = Variable
  | TypeCons
  | DataCons

-- | The term type for quoted things.
data Term
  = Var VarKind Text
  | Const Lit
  | App Term Term
  | Binder Binder Term
  | Hol

-- | A term in the left hand side of an
-- equation. It's used for patterns in
-- pattern match.
data Pat
  = PVar Text
  -- | Wild card patterns
  | PWild
  -- | Literals
  | PConst Lit
  -- | Pattern for constructors
  | PCons Text [Pat]
  -- | Pattern that is created through instantiation
  -- it's not available like in the case of
  -- @
  -- data Imf : B → Type where
  --   imf : (x : A) → Imf (f x)
  --
  -- invf : (y : B ) → Imf y → A
  -- invf (f x) (imf x) = x
  -- --   ^^^^^ This part is a term that is unified but
  -- --         cannot be accessed because we cannot pattern
  -- --         match on functions.
  -- @
  | PInaccessible Term 

-- Top level

-- | The definition of an inductive family:
-- a collection of data construtors for a single
-- type constructor.
data Family = Family
  { fName :: Text
  , fParams :: [(Text, Term)]
  , fType :: Term
  , fCons :: [(Text, [(Text, Term)])]
  }

-- | A definition with one or more equations
data Declaration = Declaration
  { dName :: Text
  , dType :: Term
  , dEquations :: [([Pat], Term)]
  }

