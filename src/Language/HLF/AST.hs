{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Language.HLF.AST where
import Bound
import Control.Applicative
import Control.Monad       (ap)
import Data.Foldable
import Data.Traversable
import Prelude.Extras

data Term a = Star -- Kind of types
            | Pi (Term a) (Scope () Term a)
            | Lam (Scope () Term a) (Term a)
            | Var a
            | Term a :@: Term a
            deriving (Eq, Show, Functor, Foldable, Traversable)
instance Show1 Term
instance Eq1 Term

instance Applicative Term where
  pure = Var
  (<*>) = ap
instance Monad Term where
  return = Var
  Star >>= _ = Star
  Pi ty body >>= f = Pi (ty >>= f) (body >>>= f)
  Var a >>= f = f a
  (fun :@: a) >>= f = (fun >>= f) :@: (a >>= f)
  Lam l ty >>= f = Lam (l >>>= f) (ty >>= f)

lam :: Eq a => a -> Term a -> Term a -> Term a
lam a f = Lam (abstract1 a f)

piTy :: Eq a => Term a -> a -> Term a -> Term a
piTy ty a f = Pi ty (abstract1 a f)
