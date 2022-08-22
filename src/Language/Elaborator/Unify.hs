module Language.Elaborator.Unify
  ( unify
  ) where

import qualified Data.HashMap.Strict as HashMap

import Language.Elaborator.Eval (evalApp, quote)
import Language.Elaborator.Types
import Language.Elaborator.Context
import Control.Exception (throwIO)

data Error = Mismatch
  deriving stock Show

instance Exception Error where

forceComp :: Ctx -> Value -> IO Value
forceComp Ctx{..} = \case
  v@(VTop name spine) -> case HashMap.lookup name cDefs of
    Just (Just val, _) -> pure (foldl' evalApp val spine)
    _ -> pure v
  v@(VFlex metaVar spine) -> readIORef metaVar <&> \case
    Empty _ -> v
    Full f -> foldl' evalApp f spine
  v -> pure v


unify :: Ctx -> Value -> Value -> IO ()
unify ctx left right =
    catch (go ctx left right) $ \case
      Mismatch -> error $ "mismatch between '" <> ppTerm [] (quote 0 left) <> "' and '" <> ppTerm [] (quote 0 right) <> "'"
      err      -> throwIO err

  where
    goSpine :: Ctx -> [Value] -> [Value] -> IO ()
    goSpine _ [] [] = pass
    goSpine ctx (l : ls) (r : rs) = go ctx l r *> goSpine ctx ls rs
    goSpine _ _ _ = error "spine unification"

    go :: Ctx -> Value -> Value -> IO ()
    go ctx@Ctx{..} lhs rhs = do
      lhs' <- forceComp ctx lhs
      rhs' <- forceComp ctx rhs
      case (lhs', rhs') of
        (VType, VType) -> pass
        (VRigid idx1 spine1, VRigid idx2 spine2)
          | idx1 == idx2 -> goSpine ctx spine1 spine2
        (VFlex hole1 spine1, VFlex hole2 spine2)
          | hole1 == hole2 -> goSpine ctx spine1 spine2
        (VFlex hole spine, t) -> readIORef hole >>= \case
          Empty holeLvl -> solve (Lvl cLevel) holeLvl hole spine t
          Full holeValue -> go ctx holeValue t
        (t, VFlex hole spine) -> readIORef hole >>= \case
          Empty holeLvl -> solve (Lvl cLevel) holeLvl hole spine t
          Full holeValue -> go ctx t holeValue
        (VTop name1 spine1, VTop name2 spine2)
          | name1 == name2 -> goSpine ctx spine1 spine2
        (VLam var1 func1, VLam _var2 func2) ->
          go (addVar ctx var1 (vVar (Lvl cLevel))) (func1 $ vVar (Lvl cLevel)) (func2 $ vVar (Lvl cLevel))
        (VLam var1 func, t) ->
          go (addVar ctx var1 (vVar (Lvl cLevel))) (func $ vVar (Lvl cLevel)) (evalApp t $ vVar (Lvl cLevel))
        (t, VLam var1 func) ->
          go (addVar ctx var1 (vVar (Lvl cLevel))) (evalApp t $ vVar (Lvl cLevel)) (func $ vVar (Lvl cLevel))
        (VPi var1 typ1 func1, VPi _var2 typ2 func2) -> do
          go ctx typ1 typ2
          go (addVar ctx var1 (vVar (Lvl cLevel))) (func1 $ vVar (Lvl cLevel)) (func2 $ vVar (Lvl cLevel))
        (_, _) -> throwIO Mismatch

solve :: Lvl -> Lvl -> IORef Hole -> [Value] -> Value -> IO ()
solve lvl holeLvl hole spine t
  | null spine = do
    occurs lvl t
    writeIORef hole $ Full t
  | otherwise  = error "not implemented yet"
  where
    occurs :: Lvl -> Value -> IO ()
    occurs scope = \case
      VType -> pass
      VRigid (flip idxToLvl scope -> lvl') spine'
        | lvl' > holeLvl -> error "the type escaped its scope"
        | otherwise      -> traverse_ (occurs scope) spine'
      VFlex metaVar spine'
        | metaVar == hole -> error "occurs"
        | otherwise       -> do
          readIORef metaVar >>= \case
            Empty _ -> pass
            Full value -> occurs scope value
          traverse_ (occurs scope) spine'
      -- TODO: Need to verify whether we need to reduce VTop because maybe there
      -- is an 'a' inside
      VTop _label spine' -> traverse_ (occurs scope) spine'
      VLam _name body -> occurs (scope + 1) (body (vVar scope))
      VPi _var typ body -> do
        occurs scope typ
        occurs (scope + 1) (body (vVar scope))
