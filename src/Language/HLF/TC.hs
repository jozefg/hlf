module Language.HLF.TC where
import           Bound
import           Control.Monad (guard)
import qualified Data.Foldable     as T
import qualified Data.Map          as M
import           Data.Maybe
import           Language.HLF.AST
import           Language.HLF.Eval

assert :: [Maybe a] -> b -> Maybe b
assert as b = T.sequence_ as >> return b

unBind :: Int -> Scope () Term Fresh -> Term Fresh
unBind i sc = instantiate1 (Var (Unbound i)) sc

typeTerm :: Int -> Context -> Term Fresh -> Maybe (Term Fresh)
typeTerm i cxt t = case t of
  Star -> Just Star
  Var j -> M.lookup j cxt
  Lam body argTy ->
    let cxt' = M.insert (Unbound i) argTy cxt
    in typeTerm (i + 1) cxt' (unBind i body)
  f :@: a ->
    case typeTerm i cxt f of
     Just (Pi ty retTy) ->
       assert [checkTerm i cxt a ty] (instantiate1 (nf a) retTy)
     _ -> Nothing
  Pi ty body ->
    let val = nf ty
        cxt' = M.insert (Unbound i) val cxt
    in assert [ checkTerm i cxt ty Star
              , checkTerm (i + 1) cxt' (unBind i body) Star] Star

checkTerm :: Int -> Context -> Term Fresh -> Term Fresh -> Maybe ()
checkTerm i cxt term ty = do
  ty' <- typeTerm i cxt term
  guard (nf ty' == nf ty)

typeProgram :: Context -> Bool
typeProgram cxt = T.all (isJust . typeTerm 0 cxt) cxt
