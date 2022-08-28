-- | Module for evaluation and quotation
-- between the PreSyntax and the Syntax.
module Language.Core.Conversion (
  forceValue,
  unbind,
  eval,
  quote,
  applyArg
) where

import Language.Core.Syntax
import Language.Core.Context (MonadTy, LocalCtx(level, env), addName)

import qualified Unsafe
import GHC.IO (unsafePerformIO)

-- | Useful to run an NBE term with a fresh var
unbind :: MonadTy m => (Value -> Value) -> m Value
unbind body = do
  Lvl curLvl <- asks level
  pure $ body (VVar (Idx curLvl))

applyArg :: Value -> Value -> Value
applyArg fun arg = case fun of
  VStuck reason spine   -> VStuck reason (spine ++ [arg]) -- TODO: Fix this inneficience
  VBinder (VLam _) func -> func arg
  _                     -> error "Compiler Error: Should not be possible to apply this term"

-- TODO: Use a better data structure probably? I'm not
-- sure if i should only lists to represent the spine.
applySpine :: Value -> Spine -> Value
applySpine = foldl' applyArg

-- Force the week head normal form if one of the expressions
-- can be instantiated because it's a hole that now is filled
-- with some sub term.
forceValue :: Value -> IO Value
forceValue = \case
  val@(VStuck (Flex hole) spine) -> readIORef hole <&> \case
    Filled va -> applySpine va spine
    Empty _   -> val
  other -> pure other

-- Evaluation of a term into a value

evalBinder :: LocalCtx -> Binder -> (Text, VBinder)
evalBinder ctx = \case
  Lam txt         -> (txt, VLam txt)
  Let txt te' te2 -> (txt, VLet txt (unsafeEval ctx te') (unsafeEval ctx te2))
  Pi txt te'      -> (txt, VPi txt (unsafeEval ctx te'))

unsafeEval :: LocalCtx -> Term -> Value
unsafeEval ctx = \case
  Free text      -> VStuck (Top text) []
  Hol            -> VStuck (Flex (unsafePerformIO (newIORef (Empty (Lvl 0))))) [] -- Creates a hole at top level
  Var (Idx idx)  -> snd (env ctx Unsafe.!! idx)
  Lit lit        -> VLit lit
  App te te'     -> applyArg (unsafeEval ctx te) (unsafeEval ctx te')
  Binder bi te   -> let (name, binder) = evalBinder ctx bi
                    in VBinder binder (\arg -> unsafeEval (addName name arg ctx) te)

eval :: LocalCtx -> Term -> IO Value
eval ctx term = pure (unsafeEval ctx term)

-- Quotation of a value to a term

quoteStuck :: Lvl -> Stuck -> Term
quoteStuck lvl = \case
  Flex _ir   -> Hol
  Rigid idx  -> Var (lvlToIdx lvl idx)
  Top txt    -> Free txt

quoteBinder :: Lvl -> VBinder -> Binder
quoteBinder lvl = \case
  VLam txt        -> Lam txt
  VLet txt va va' -> Let txt (quote lvl va) (quote lvl va')
  VPi txt va      -> Pi txt (quote lvl va)

quote :: Lvl -> Value -> Term
quote lvl@(Lvl lvlNum) = \case
  VLit lit        -> Lit lit
  VStuck st spine -> let var = quoteStuck lvl st
                     in foldl' App var (map (quote lvl) spine)
  VBinder vb term -> let binder = quoteBinder lvl vb
                     in Binder binder (quote (lvl + 1) (term (VVar (Idx lvlNum))))
