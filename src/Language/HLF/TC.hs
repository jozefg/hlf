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

bind :: Int -> Term Fresh -> Context -> Context
bind = M.insert . Unbound

typeTerm :: Int -> Context -> Term Fresh -> Maybe (Term Fresh)
typeTerm i cxt t = case t of
  Star -> Just Star
  Var j -> M.lookup j cxt
  Lam body argTy -> typeTerm (i + 1) (bind i argTy cxt) $ unBind i body
  f :@: a ->
    case typeTerm i cxt f of
     Just (Pi ty retTy) ->
       assert [checkTerm i cxt a $ nf ty] (instantiate1 (nf a) retTy)
     Just (When r l) ->
       assert [checkTerm i cxt a $ nf l] (nf r)
     _ -> Nothing
  Pi ty body ->
    assert [ isType i cxt ty
           , isType (i + 1) (bind i (nf ty) cxt) (unBind i body)]
    Star
  When r l -> assert [isType i cxt $ nf l, isType i cxt $ nf r]
              Star

checkTerm :: Int -> Context -> Term Fresh -> Term Fresh -> Maybe ()
checkTerm i cxt term ty = do
  ty' <- typeTerm i cxt term
  guard (nf ty' == nf ty)

isType :: Int -> Context -> Term Fresh -> Maybe ()
isType i cxt ty = checkTerm i cxt ty Star

typeProgram :: Context -> Bool
typeProgram cxt = T.all (isJust . typeTerm 0 cxt) cxt
