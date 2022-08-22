module Language.Elaborator.Context
  ( Ctx(..)
  , emptyCtx
  , addVar
  , define
  ) where

import Data.HashMap.Strict qualified as HashMap

import Language.Elaborator.Tree (Name)
import Language.Elaborator.Types (Value)


data Ctx = Ctx
  { cTypes :: HashMap Name (Int, Value)
  , cDefs  :: HashMap Name (Maybe Value, Value)
  , cLevel :: Int
  , cEnv   :: [Value]
  }

emptyCtx :: Ctx
emptyCtx = Ctx
  { cTypes = HashMap.empty
  , cDefs  = HashMap.empty
  , cLevel = 0
  , cEnv = []
  }

addVar :: Ctx -> Name -> Value -> Ctx
addVar ctx@Ctx{..} name typ = ctx
  { cTypes = HashMap.insert name (cLevel, typ) cTypes
  , cLevel = cLevel + 1
  , cEnv = typ : cEnv
  }

define :: Ctx -> Name -> Maybe Value -> Value -> Ctx
define ctx name fun value = ctx
  { cDefs = HashMap.insert name (fun, value) (cDefs ctx)
  }