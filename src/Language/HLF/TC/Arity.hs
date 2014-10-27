{-# LANGUAGE FlexibleContexts #-}
module Language.HLF.TC.Arity where
import           Bound
import           Control.Applicative
import           Control.Lens         hiding (Context)
import           Control.Monad.Gen
import           Control.Monad.Reader
import           Data.Foldable
import qualified Data.Map             as M
import           Data.Maybe
import           Language.HLF.AST
import           Language.HLF.TC.Util

arityBound :: (Functor m, MonadReader ArityMap m, MonadGen Int m)
              => (Term Fresh -> m Int)
              -> Term Fresh -> Scope () Term Fresh -> m Int
arityBound f ty scope = do
  tyArity <- typeArity ty
  i <- gen
  let term = instantiate1 (Var $ Unbound i) scope
  local (at (Unbound i) .~ Just tyArity) $ f term

-- See note 2
termArity :: (Functor m, MonadReader ArityMap m, MonadGen Int m)
             => Term Fresh -> m Int
termArity t = case t of
  Lam scope ty -> arityBound termArity ty scope -- See Note 1
  Var a -> view (at a . to fromJust)
  l :@: _ -> pred <$> termArity l
  When{} -> return 0
  Pi{} -> return 0
  Star -> return 0

-- See note 2
typeArity :: (Functor m, MonadReader ArityMap m, MonadGen Int m)
             => Term Fresh -> m Int
typeArity t = case t of
  When _ l -> succ <$> typeArity l
  Pi ty scope -> succ <$> arityBound typeArity ty scope
  Var a -> view (at a . to fromJust)
  Star -> return 0
  Lam{} -> return 0
  _ :@: _ -> return 0

buildArityMap :: Context -> ArityMap
buildArityMap = foldl' build M.empty . M.toList
  where build aMap (i, term) = M.insert i (runGenT (typeArity term) aMap) aMap

-- Note 1: It's not entirely obvious why we can disregard the addition
-- of a lambda on a term. The reason is awfully hacky and makes me
-- feel bad. We're only concerened with beta-normal forms at this
-- point in typechecking. This means that all types are of the form
-- `Variable|Constant :@: term :@: term ..` or `Lam ...`. When we're
-- checking whether a term has an arity of zero, we really don't want
-- to include lambdas as a +1 even though they're obviously increasing
-- the arity. Why? Because in this case they're actually being used to
-- reduce the arity of the term they're closing over. Overall this
-- leaves me with a bad taste in my mouth even though I'm reasonably
-- certain I'm correct. I guess this is more apology letter than
-- comment.

-- Note 2: To clarify the difference between these. `typeArity` takes
-- a term representing a type and returns a number representing the
-- number of arguments a value of that type would take. `termArity`
-- takes a term representing a value and says how many arguments are
-- "left" so to speak for such a type. If the latter number is not
-- zero, then we've got a problem.
