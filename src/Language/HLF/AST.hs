{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Language.HLF.AST where
import           Bound
import           Control.Applicative
import           Control.Monad       (ap)
import           Data.Foldable
import qualified Data.Map            as M
import           Data.String
import qualified Data.Text           as T
import           Data.Traversable
import           Prelude.Extras


data Term a = Star
            | Pi (Term a) (Scope () Term a)
            | When (Term a) (Term a) -- See note 1
            | Lam (Scope () Term a) (Term a)
            | Var a
            | Term a :@: Term a
            deriving (Eq, Show, Functor, Foldable, Traversable)
instance Show1 Term
instance Eq1 Term

instance IsString a => IsString (Term a) where
  fromString = Var . fromString

instance Applicative Term where
  pure = Var
  (<*>) = ap
instance Monad Term where
  return = Var
  Star >>= _ = Star
  Pi ty body >>= f = Pi (ty >>= f) (body >>>= f)
  When r l >>= f = When (r >>= f) (l >>= f)
  Var a >>= f = f a
  (fun :@: a) >>= f = (fun >>= f) :@: (a >>= f)
  Lam l ty >>= f = Lam (l >>>= f) (ty >>= f)


data Fresh = Free Int | Unbound Int
           deriving(Eq, Show, Ord)
type Context = M.Map Fresh (Term Fresh)
type Name = T.Text


data Definition a = (:=) { defName :: a
                         , defTy   :: Term a }
                  deriving(Show, Functor, Foldable, Traversable)
infixr 0 :=

data Polarity = Pos | Neg
              deriving (Show)

data TopLevel a = Mode a [Polarity]
                | TypeFamily (Definition a) [Definition a]
                deriving(Show, Functor, Foldable, Traversable)

type Program = [TopLevel Name]

fresh2name :: Fresh -> Name
fresh2name = T.pack . show

lam :: Eq a => a -> Term a -> Term a -> Term a
lam a f = Lam (abstract1 a f)

piTy :: Eq a => Term a -> a -> Term a -> Term a
piTy ty a f = Pi ty (abstract1 a f)

piMany :: Eq a => [(Term a, a)] -> Term a -> Term a
piMany vars body = Data.Foldable.foldr (uncurry piTy) body vars

(<--) :: Term a -> Term a -> Term a
(<--) = When
infixl 0 <--

(-->) :: Term a -> Term a -> Term a
l --> r = Pi l (abstract (const Nothing) r)
infixr 0 -->

-- Note 1: This actually needs its own data constructor. The moded
-- checker (when it actually exists) will work from top to bottom so
-- this critically affects the modality of variables. This isn't true
-- of a forwards arrow which is just a degerenerate pi type.
