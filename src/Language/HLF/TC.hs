module Language.HLF.TC where
import           Bound
import           Control.Monad
import qualified Data.Map          as M
import           Language.HLF.AST
import           Language.HLF.Eval

type Fresh = Int
type Context = M.Map Int (Term Fresh)

typeTerm :: Fresh -> Context -> Term Fresh -> Maybe (Term Fresh)
typeTerm i cxt t = case t of
  Star -> Just Star
  Nat -> Just Star
  Zero -> Just Nat
  Succ e -> checkTerm i cxt e Nat >> return Nat
  Var j -> M.lookup j cxt
  Lam body argTy ->
    let cxt' = M.insert i argTy cxt
        body' = instantiate1 (Var i) body
    in typeTerm (i + 1) cxt' body'
  f :@: a ->
    case typeTerm i cxt f of
     Just (Pi ty retTy) ->
       checkTerm i cxt a ty >> return (instantiate1 (nf a) retTy)
     _ -> Nothing
  Pi ty body -> do
    let val = nf ty
        cxt' = M.insert i val cxt
        body' = instantiate1 (Var i) body
    checkTerm i cxt ty Star
    checkTerm (i + 1) cxt' body' Star
    return Star

checkTerm :: Fresh -> Context -> Term Fresh -> Term Fresh -> Maybe ()
checkTerm i cxt term ty = do
  ty' <- typeTerm i cxt term
  guard (nf ty' == nf ty)

typeOf :: Term Fresh -> Maybe (Term Fresh)
typeOf = typeTerm 0 M.empty
