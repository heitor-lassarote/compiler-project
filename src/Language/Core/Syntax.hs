-- | The definition of the evaluation syntax
-- it's almost like an interpreter so probably
-- in the future we will try to make something with LLVM
-- The "Value" type represents the neutral form
-- of an computation.
module Language.Core.Syntax (
  Idx(..), Lvl(..), lvlToIdx, idxToLvl,
  Binder(..), Lit(..), Term(..), ppTerm,
  Hole(..), Stuck(..), VBinder(..), Value(..),
  pattern VVar, Meta, Spine,
) where

import qualified Unsafe

newtype Idx = Idx Int deriving newtype (Num, Eq, Ord)
newtype Lvl = Lvl Int deriving newtype (Num, Eq, Ord)

lvlToIdx :: Lvl -> Idx -> Idx
lvlToIdx (Lvl l) (Idx x) = Idx (l - x - 1)

idxToLvl :: Idx -> Lvl -> Lvl
idxToLvl (Idx x) (Lvl l) = Lvl (l + x + 1)

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

-- | The term type for quoted things.
data Term
  = Var Idx
  | Free Text
  | Lit Lit
  | App Term Term
  | Binder Binder Term
  | Hol

-- Values

type Spine = [Value]

type Meta = IORef Hole

data Hole
  = Empty Lvl
  | Filled Value

-- | An application that is stuck by
-- one of the reasons below
data Stuck
  = Flex Meta
  | Rigid Idx
  | Top Text

data VBinder
  = VLam Text
  | VLet Text Value Value
  | VPi  Text Value

data Value
  = VLit Lit
  | VStuck Stuck Spine
  | VBinder VBinder (Value -> Value)

pattern VVar :: Idx -> Value
pattern VVar i = VStuck (Rigid i) []

parens :: Text -> Text
parens t = "(" <> t <> ")"

ppTermBinder :: [Text] -> Binder -> (Text, Text)
ppTermBinder names = \case
  Lam txt         -> (txt, txt <> " => ")
  Let txt te' te2 -> (txt, "let " <> txt <> " : " <> ppTerm names te' <> " = " <> ppTerm names te2 <> ";")
  Pi "_" te'      -> ("_", ppTerm names te' <> " -> ")
  Pi txt te'      -> (txt, parens (txt <> " : " <> ppTerm names te') <> " -> ")

ppTermLit :: Lit -> Text
ppTermLit = \case
  Str txt -> show txt
  Int n   -> show n
  Typ     -> "Type"

ppTerm :: [Text] -> Term -> Text
ppTerm names = \case
    Var (Idx idx) -> names Unsafe.!! idx
    Free txt      -> "!" <> txt
    Lit lit       -> ppTermLit lit
    App te te'    -> parens (ppTerm names te <> " " <> ppTerm names te')
    Binder bi te  -> let (name, binder) = ppTermBinder names bi
                     in parens (binder <> ppTerm (name: names) te)
    Hol           -> "_"