module Language.Core.Unify (
  unify
) where

import Language.Core.Context (LocalCtx (..))
import Language.Core.Conversion (quote, forceValue, applyArg)
import Language.Core.Syntax

import Control.Exception (throwIO)

data Error = Mismatch
  deriving stock Show

instance Exception Error where

unify :: LocalCtx -> Value -> Value -> IO ()
unify ctx' left right =
    catch (go (level ctx') left right) $ \case
      Mismatch -> error $ "mismatch between '" <> ppTerm [] (quote 0 left) <> "' and '" <> ppTerm [] (quote 0 right) <> "'"
  where
    toIdx :: Lvl -> Idx
    toIdx (Lvl i) = Idx i

    goSpine :: Lvl -> [Value] -> [Value] -> IO ()
    goSpine _ [] [] = pass
    goSpine ctx (l : ls) (r : rs) = go ctx l r *> goSpine ctx ls rs
    goSpine _ _ _ = error "spine unification"

    var = VVar . toIdx

    go :: Lvl -> Value -> Value -> IO ()
    go level lhs rhs = do
      lhs' <- forceValue lhs
      rhs' <- forceValue rhs
      case (lhs', rhs') of
        (VBinder (VLam _) term, VBinder (VLam _) term') -> go (level + 1) (term (var level)) (term' (var level))
        (VBinder (VLam _) term, t) -> go (level + 1) (term (var level)) (t `applyArg` var level)
        (t, VBinder (VLam _) term) -> go (level + 1) (t `applyArg` var level) (term (var level))
        (VLit Typ, VLit Typ) -> pass

        (VBinder (VPi _ t) term, VBinder (VPi _ t') term') -> do
          go level t t'
          go (level + 1) (term (var level)) (term' (var level))

        (VStuck (Flex hole) b, VStuck (Flex hole') b') | hole == hole' -> goSpine level b b'
        (VStuck (Rigid idx) b, VStuck (Rigid idx') b') | idx  == idx'  -> goSpine level b b'
        (VStuck (Top name) b , VStuck (Top name') b')  | name == name' -> goSpine level b b'

        (VStuck (Flex hole) spine, t) ->  readIORef hole >>= \case
          Empty holeLvl -> solve level holeLvl hole spine t
          Filled res    -> go level res t

        (t, VStuck (Flex hole) spine) ->  readIORef hole >>= \case
          Empty holeLvl -> solve level holeLvl hole spine t
          Filled res    -> go level res t

        (_, _) -> throwIO Mismatch

solve :: Lvl -> Lvl -> Meta -> [Value] -> Value -> IO ()
solve lvl holeLvl hole spine t
  | null spine = occurs lvl t *> writeIORef hole (Filled t)
  | otherwise  = error "not implemented yet"
  where
    occurs :: Lvl -> Value -> IO ()
    occurs scope@(Lvl i) = \case
      VLit _ -> pass
      VStuck binder spine' -> do
        case binder of
          Rigid idx | idxToLvl idx holeLvl > holeLvl -> error "The type escaped its scope"
          Flex hole' | hole == hole' -> error "Occurs check"
          Flex hole' -> readIORef hole' >>= \case
             Empty _   -> pass
             Filled va -> occurs scope va
          _ -> pure ()
        traverse_ (occurs scope) spine'
      VBinder binder body -> do
        case binder of
          VLam _        -> pass
          VLet _ va va' -> occurs scope va >> occurs scope va'
          VPi _ va      -> occurs scope va
        occurs (scope + 1) (body (VVar (Idx i)))