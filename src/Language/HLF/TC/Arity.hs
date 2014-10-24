module Language.HLF.TC.Arity where
import           Bound
import           Data.Foldable
import qualified Data.Map             as M
import           Language.HLF.AST
import           Language.HLF.TC.Util

termArityBound :: Int -> ArityMap -> Term Fresh -> Scope () Term Fresh -> Int
termArityBound i aMap ty scope = termArity (i + 1) aMap' term
  where term = instantiate1 (Var $ Unbound i) scope
        aMap' = M.insert (Unbound i) (typeArity i aMap ty) aMap

typeArityBound :: Int -> ArityMap -> Term Fresh -> Scope () Term Fresh -> Int
typeArityBound i aMap ty scope = typeArity (i + 1) aMap' term
  where term = instantiate1 (Var $ Unbound i) scope
        aMap' = M.insert (Unbound i) (typeArity i aMap ty) aMap

-- See note 2
termArity :: Int -> ArityMap -> Term Fresh -> Int
termArity i aMap t = case t of
  Lam scope ty -> termArityBound i aMap ty scope -- See Note 1
  Var a -> aMap M.! a
  l :@: _ -> termArity i aMap l - 1
  When{} -> 0
  Pi{} -> 0
  Star -> 0

-- See note 2
typeArity :: Int -> ArityMap -> Term Fresh -> Int
typeArity i aMap t = case t of
  When _ l -> 1 + typeArity i aMap l
  Pi ty scope -> 1 + typeArityBound i aMap ty scope
  Var a -> aMap M.! a
  Lam{} -> 0
  _ :@: _ -> 0
  Star -> 0

buildArityMap :: Context -> ArityMap
buildArityMap = foldl' build M.empty . M.toList
  where build aMap (i, term) = M.insert i (typeArity 0 aMap term) aMap

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
