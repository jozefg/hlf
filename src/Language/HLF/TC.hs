module Language.HLF.TC where
import           Bound
import           Control.Monad
import qualified Data.Map          as M
import           Language.HLF.AST
import           Language.HLF.Eval

type Fresh = Int
type Context = M.Map Fresh (Term Fresh)

assert :: [Maybe a] -> b -> Maybe b
assert as b = sequence_ as >> return b

unBind :: Fresh -> Scope () Term Fresh -> Term Fresh
unBind i sc = instantiate1 (Var i) sc

typeTerm :: Fresh -> Context -> Term Fresh -> Maybe (Term Fresh)
typeTerm i cxt t = case t of
  Star -> Just Star
  Var j -> M.lookup j cxt
  Lam body argTy ->
    let cxt' = M.insert i argTy cxt
    in typeTerm (i + 1) cxt' (unBind i body)
  f :@: a ->
    case typeTerm i cxt f of
     Just (Pi ty retTy) ->
       assert [checkTerm i cxt a ty] (instantiate1 (nf a) retTy)
     _ -> Nothing
  Pi ty body ->
    let val = nf ty
        cxt' = M.insert i val cxt
    in assert [ checkTerm i cxt ty Star
              , checkTerm (i + 1) cxt' (unBind i body) Star] Star

checkTerm :: Fresh -> Context -> Term Fresh -> Term Fresh -> Maybe ()
checkTerm i cxt term ty = do
  ty' <- typeTerm i cxt term
  guard (nf ty' == nf ty)

typeOf :: Term Fresh -> Maybe (Term Fresh)
typeOf = typeTerm 0 M.empty
