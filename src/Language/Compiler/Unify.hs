module Language.Compiler.Unify
  ( unify
  ) where

import Language.Compiler.Eval (evalApp)
import Language.Compiler.Types

forceComp :: Value -> IO Value
forceComp = \case
  v@(VFlex metaVar spine) -> readIORef metaVar <&> \case
    Empty _ -> v
    Full f -> foldl' evalApp f spine
  v -> pure v

unifySpine :: Lvl -> [Value] -> [Value] -> IO ()
unifySpine _ [] [] = pass
unifySpine lvl (l : ls) (r : rs) = unify lvl l r *> unifySpine lvl ls rs
unifySpine _ _ _ = error "spine unification"

unify :: Lvl -> Value -> Value -> IO ()
unify l lhs rhs = do
  lhs' <- forceComp lhs
  rhs' <- forceComp rhs
  case (lhs', rhs') of
    (VType, VType) -> pass
    (VRigid idx1 spine1, VRigid idx2 spine2)
      | idx1 == idx2 -> unifySpine l spine1 spine2
    (VFlex hole1 spine1, VFlex hole2 spine2)
      | hole1 == hole2 -> unifySpine l spine1 spine2
    (VFlex hole spine, t) -> readIORef hole >>= \case
      Empty holeLvl -> solve l holeLvl hole spine t
      Full holeValue -> unify l holeValue t
    (t, VFlex hole spine) -> readIORef hole >>= \case
      Empty holeLvl -> solve l holeLvl hole spine t
      Full holeValue -> unify l t holeValue
    (VTop name1 spine1, VTop name2 spine2)
      | name1 == name2 -> unifySpine l spine1 spine2
    (VLam _var1 func1, VLam _var2 func2) ->
      unify (l + 1) (func1 $ vVar l) (func2 $ vVar l)
    (VLam _var func, t) ->
      unify (l + 1) (func $ vVar l) (evalApp t $ vVar l)
    (t, VLam _var func) ->
      unify (l + 1) (evalApp t $ vVar l) (func $ vVar l)
    (VPi _var1 typ1 func1, VPi _var2 typ2 func2) -> do
      unify l typ1 typ2
      unify (l + 1) (func1 $ vVar l) (func2 $ vVar l)
    (_, _) -> error "mismatch"

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
            Full value -> (occurs scope) value
          traverse_ (occurs scope) spine'
      -- TODO: Need to verify whether we need to reduce VTop because maybe there
      -- is an 'a' inside
      VTop _label spine' -> traverse_ (occurs scope) spine'
      VLam _name body -> occurs (scope + 1) (body (vVar scope))
      VPi _var typ body -> do
        occurs scope typ
        occurs (scope + 1) (body (vVar scope))
