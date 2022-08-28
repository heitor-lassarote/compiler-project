-- | Module for evaluation and quotation
-- between the PreSyntax and the Syntax.
module Language.Core.Conversion (
  forceValue,
  unbind
) where

import Language.Core.Syntax (Value (..), Stuck (..), Hole (..), Spine, pattern VVar)
import Language.Core.PreSyntax (Binder(Lam))
import Language.Core.Context (MonadTy, level)

-- | Useful to run an NBE term with a fresh var
unbind :: MonadTy m => (Value -> Value) -> m Value
unbind body = do
  curLvl <- asks level
  pure $ body (VVar curLvl)

applyArg :: Value -> Value -> Value
applyArg fun arg = case fun of
  VStuck reason spine  -> VStuck reason (arg : spine)
  VBinder (Lam _) func -> func arg
  _                    -> error "Compiler Error: Should not be possible to apply this term"

-- TODO: Use a better data structure probably? I'm not
-- sure if i should only lists to represent the spine.
applySpine :: Value -> Spine -> Value
applySpine = foldr applyArg

-- Force the week head normal form if one of the expressions
-- can be instantiated because it's a hole that now is filled
-- with some sub term.
forceValue :: Value -> IO Value
forceValue = \case
  val@(VStuck (Flex hole) spine) -> readIORef hole <&> \case
    Filled va -> applySpine va spine
    Empty _   -> val
  other -> pure other