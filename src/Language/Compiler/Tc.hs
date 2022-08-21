module Language.Compiler.Tc
  ( module Language.Compiler.Tc
  ) where

import Data.HashMap.Strict qualified as HashMap

import Language.Compiler.Eval (eval)
import Language.Compiler.Tree (Expr (..), Name)
import Language.Compiler.Types hiding (Ctx, Term (..))
import Language.Compiler.Types qualified as Types
import Language.Compiler.Unify (unify)

data Ctx = Ctx
  { cTypes :: HashMap Name (Int, Value)
  , cLevel :: Int
  , cEnv :: Types.Ctx
  }

emptyCtx :: Ctx
emptyCtx = Ctx
  { cTypes = HashMap.empty
  , cLevel = 0
  , cEnv = []
  }

addVar :: Ctx -> Name -> Value -> Ctx
addVar Ctx{..} name typ = Ctx
  { cTypes = HashMap.insert name (cLevel, typ) cTypes
  , cLevel = cLevel + 1
  , cEnv = typ : cEnv
  }

infer :: Ctx -> Expr -> IO (Types.Term, Value)
infer ctx@Ctx{..} = \case
  Type -> pure (Types.Type, VType)
  Var name -> case HashMap.lookup name cTypes of
    Nothing -> fail "variable not found"
    Just (idx, typ) -> pure (Types.Var $ Idx $ cLevel - idx - 1, typ)
  Lam name expr -> do
    hole <- Types.VFlex <$> newIORef (Empty $ Lvl cLevel) <*> pure []
    (term, _typ) <- infer (addVar ctx name hole) expr
    pure (Types.Lam name term, VPi name hole $ error "TODO: can't infer lambdas yet")
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
    (_tTerm, tTyp) <- infer ctx typ
    tExpr <- check ctx expr tTyp
    pure (tExpr, tTyp)

check :: Ctx -> Expr -> Value -> IO Types.Term
check ctx@Ctx{..} expr' typ' = case (expr', typ') of
  (Lam name expr, VPi _ typ body) ->
    Types.Lam name <$> check (addVar ctx name typ) expr (body $ vVar $ Lvl cLevel)
  (t, expected) -> do
    (t', inferred) <- infer ctx t
    unify (Lvl cLevel) expected inferred
    pure t'
