module Language.Compiler.Eval
  ( eval, evalApp
  , quote
  ) where

import Unsafe qualified

import Language.Compiler.Types

eval :: Ctx -> Term -> Value
eval ctx = \case
  Type -> VType
  Var (Idx i) -> ctx Unsafe.!! i
  Free name -> VTop name []
  Lam var body -> VLam var \x -> eval (x : ctx) body
  App f x -> evalApp (eval ctx f) (eval ctx x)
  Pi var typ body -> VPi var (eval ctx typ) \x -> eval (x : ctx) body
  Ann var _typ -> eval ctx var

evalApp :: Value -> Value -> Value
evalApp f x = case f of
  VRigid idx spine -> VRigid idx (x : spine)
  VFlex metaVar spine -> VFlex metaVar (x : spine)
  VTop label spine -> VTop label (x : spine)
  VLam _name body -> body x
  _ -> error "impossible"

quote :: Lvl -> Value -> Term
quote l = \case
  VType -> Type
  VRigid idx spine -> foldl' App (Var $ lvlToIdx l idx) (map (quote l) spine)
  VFlex _metaVar spine -> foldl' App (Free "_") (map (quote l) spine)
  VTop label spine -> foldl' App (Free label) (map (quote l) spine)
  VLam name body -> Lam name (quote (l + 1) (body (vVar l)))
  VPi var typ body -> Pi var (quote l typ) (quote (l + 1) (body (vVar l)))
