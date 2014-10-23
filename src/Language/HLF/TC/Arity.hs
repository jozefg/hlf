module Language.HLF.TC.Arity where
import           Bound
import           Control.Monad.Reader
import qualified Data.Map             as M
import           Language.HLF.AST
import           Language.HLF.Error
import           Language.HLF.TC.Util

arityBound :: Int -> ArityMap -> Term Fresh -> Scope () Term Fresh -> Int
arityBound i aMap ty scope = arity (i + 1) aMap' term
  where term = instantiate1 (Var $ Unbound i) scope
        aMap' = M.insert (Unbound i) (arity i aMap ty) aMap

arity :: Int -> ArityMap -> Term Fresh -> Int
arity i aMap t = case t of
  When r _ -> 1 + arity i aMap r
  Pi ty scope -> arityBound i aMap ty scope
  Lam scope ty -> arityBound i aMap ty scope
  Var a -> aMap M.! a
  l :@: _ -> arity i aMap l - 1
  Star -> 0

appChain :: Term a -> (Term a, [Term a])
appChain = go []
  where go chain (l :@: r) = go (r : chain) l
        go chain l = (l, chain)

buildArityMap :: Context -> ContextM ArityMap
buildArityMap = foldM build M.empty . M.toList
  where build arities (i, term) = undefined
