module Language.HLF.TC.Arity where
import           Bound
import           Control.Monad.Reader
import qualified Data.Map             as M
import           Language.HLF.AST
import           Language.HLF.Error
import           Language.HLF.TC.Util

arityBound :: ArityMap -> Term Fresh -> Scope () Term Fresh -> Int
arityBound = undefined

arity :: ArityMap -> Term Fresh -> Int
arity aMap t = case t of
  When r _ -> 1 + arity aMap r
  Pi ty scope -> arityBound aMap ty scope
  Lam scope ty -> arityBound aMap ty scope
  Var a -> aMap M.! a
  Star -> 0
  l :@: _ -> arity aMap l - 1

appChain :: Term a -> (Term a, [Term a])
appChain = go []
  where go chain (l :@: r) = go (r : chain) l
        go chain l = (l, chain)

buildArityMap :: Context -> ContextM ArityMap
buildArityMap = foldM build M.empty . M.toList
  where build arities (i, term) = undefined
