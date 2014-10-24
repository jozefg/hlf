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


termArity :: Int -> ArityMap -> Term Fresh -> Int
termArity i aMap t = case t of
  Lam scope ty -> termArityBound i aMap ty scope
  Var a -> aMap M.! a
  l :@: _ -> termArity i aMap l - 1
  When{} -> 0
  Pi{} -> 0
  Star -> 0

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
