module Language.Core.Context (
  GlobalCtx(..),
  LocalCtx(..),
  MonadTy
) where

import Language.Core.Syntax ( Lvl, Value, Meta )

type Telescope = [(Text, Value)]

data GlobalCtx = GlobalCtx
  { holes :: HashMap Text Meta
  }

data LocalCtx = LocalCtx
  { ctx :: Telescope
  , level :: Lvl
  }

type MonadTy m = (MonadState GlobalCtx m, MonadReader LocalCtx m)