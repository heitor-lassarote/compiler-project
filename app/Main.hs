{-# LANGUAGE BangPatterns #-}
module Main (main) where
import Language.Core.Syntax (ppTerm, Binder (..), Term (..), Stuck (..), Value (..), Hole (Filled))
import Language.Core.Context (LocalCtx(LocalCtx))
import Language.Core.Conversion

-- x => y => z => x
lam = Binder (Lam "x") (Binder (Lam "y") (Binder (Lam "z") (Var 2)))

-- f => f a b c
fun = Binder (Lam "f") (App (App (App (Var 0) (Free "a")) (Free "b")) (Free "c"))

-- (f => f a b c) (x => y => z => x) = a

term = App fun lam

ctx = LocalCtx [] 0

main :: IO ()
main = do
  evalFun <- eval ctx fun
  evalLam <- eval ctx lam
  lelHol  <- eval ctx Hol

  let g = applyArg evalFun lelHol

  case lelHol of
    VStuck (Flex hole) [] -> do
      writeIORef hole (Filled evalLam)
      pure ()
    _ -> pure ()

  g' <- forceValue g

  putTextLn ("Run: " <> ppTerm [] (quote 0 g'))
  putTextLn (ppTerm [] (quote 0 evalLam))

  let !nf = applyArg evalFun evalLam
  e <- eval ctx term

  putTextLn (ppTerm [] (quote 0 nf))
  putTextLn (ppTerm [] (quote 0 e))