module Language.Core.Context (
  GlobalCtx(..),
  LocalCtx(..),
  MonadTy,
  addName
) where

import Language.Core.Syntax
import qualified Data.HashMap.Strict as HashMap

type Telescope = [(Text, Value)]

data GlobalCtx = GlobalCtx
  { holes :: HashMap Text Meta
  , signatures :: HashMap Text Value
  }

data LocalCtx = LocalCtx
  { env   :: Telescope
  , level :: Lvl
  }

addName :: Text -> Value -> LocalCtx -> LocalCtx
addName name val ctx' =
  ctx' { env = (name, val) : env ctx', level = level ctx' + 1 }

type MonadTy m = (MonadState GlobalCtx m, MonadReader LocalCtx m)