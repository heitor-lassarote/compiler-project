module Language.Elaborator.Tc
  ( module Language.Elaborator.Tc
  ) where

import Data.HashMap.Strict qualified as HashMap

import Language.Elaborator.Eval (eval, quote)
import Language.Elaborator.Tree (Expr (..))
import Language.Elaborator.Types (Value(..), Hole(..), Idx(..), Lvl(..), vVar, ppTerm)
import Language.Elaborator.Types qualified as Types
import Language.Elaborator.Unify (unify)
import Language.Elaborator.Context (Ctx(..), addVar, define)

newHole :: Ctx -> IO (IORef Hole)
newHole Ctx {cLevel} = newIORef (Empty $ Lvl cLevel)

infer :: Ctx -> Expr -> IO (Types.Term, Value)
infer ctx@Ctx{..} = \case
  Typ -> pure (Types.Type, VType)
  Var name -> case HashMap.lookup name cTypes of
    Nothing -> case HashMap.lookup name cDefs of
      Nothing       -> fail "variable not found"
      Just (_, typ) -> pure (Types.Free name, typ)
    Just (idx, typ) -> pure (Types.Var $ Idx $ cLevel - idx - 1, typ)
  Lam {} -> error "TODO: can't infer lambdas yet"
  App f x -> do
    (fTerm, fTyp) <- infer ctx f
    case fTyp of
      VPi _name typ body -> do
        xTerm <- check ctx x typ
        pure (Types.App fTerm xTerm, body $ eval cEnv xTerm)
      _ -> error "not a function type"
  Pi name typ body -> do
    tTyp <- check ctx typ VType
    tBody <- check (addVar ctx name $ eval cEnv tTyp) body VType
    pure (Types.Pi name tTyp tBody, VType)
  Ann expr typ -> do
    (tTerm, tTyp) <- infer ctx typ
    putTextLn $ "ann: " <> (ppTerm [] tTerm)
    let evaluatedTy = eval cEnv tTerm
    tExpr <- check ctx expr evaluatedTy
    pure (tExpr, evaluatedTy)
  Let x ty val andThen -> do
    tyElab <- check ctx ty VType
    let tySem = eval cEnv tyElab
    valElab <- check ctx val tySem
    let ctx' = define ctx x (Just $ eval cEnv valElab) tySem
    (thenElab, resTy) <- infer ctx' andThen
    pure (Types.Let x tyElab valElab thenElab, resTy)
  Hol -> do
    hole1 <- newHole ctx
    hole2 <- newHole ctx
    pure (Types.Hol hole1, VFlex hole2 [])

check :: Ctx -> Expr -> Value -> IO Types.Term
check ctx@Ctx{..} expr' typ' = case (expr', typ') of
  (Lam name expr, VPi _ typ body) ->
    Types.Lam name <$> check (addVar ctx name typ) expr (body $ vVar $ Lvl cLevel)
  (t, expected) -> do
    (t', inferred) <- infer ctx t
    unify ctx expected inferred
    pure t'
