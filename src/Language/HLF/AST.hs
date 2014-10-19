{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.HLF.AST (FreeVar, Term(..)) where
import Bound
import Control.Applicative
import Control.Monad       (ap)
import Data.Foldable
import Data.Traversable
import Prelude.Extras

newtype FreeVar = FreeVar Int
                deriving (Eq, Ord, Enum)

instance Show FreeVar where
  show (FreeVar i) = 'x' : show i

data Term a = Ann (Term a) (Term a)
            | Star -- Kind of types
            | Pi (Term a) (Scope FreeVar Term a)
            | Lam (Scope FreeVar Term a) (Term a)
            | Var a
            | Term a :@: (Term a)
            | Zero
            | Succ (Term a)
            | Nat
            deriving (Eq, Show, Functor, Foldable, Traversable)
instance Show1 Term
instance Eq1 Term

instance Applicative Term where
  pure = Var
  (<*>) = ap
instance Monad Term where
  return = Var
  Ann ty scope >>= f = Ann (ty >>= f) (scope >>= f)
  Star >>= _ = Star
  Pi ty body >>= f = Pi (ty >>= f) (body >>>= f)
  Var a >>= f = f a
  (fun :@: a) >>= f = (fun >>= f) :@: (a >>= f)
  Zero >>= _ = Zero
  Succ a >>= f = Succ (a >>= f)
  Nat >>= _ = Nat
  Lam l ty >>= f = Lam (l >>>= f) (ty >>= f)
