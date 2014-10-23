{-# LANGUAGE LambdaCase #-}
module Language.HLF.TC.Actual where
import           Bound
import           Control.Lens         hiding (Context)
import           Control.Monad.Reader
import           Data.Monoid
import           Language.HLF.AST
import           Language.HLF.Error
import           Language.HLF.Normalize
import           Language.HLF.TC.Util

typeTerm :: Int -> Term Fresh -> TyM (Term Fresh)
typeTerm i t = do
  namedT <- addNames t
  local (errorCxt . termExpr .~ Just namedT) $
    case t of
     Star -> return Star -- See note 1
     Var j -> lookupVar j
     Lam body argTy -> bind i argTy . typeTerm (i + 1) $ unBind i body
     f :@: a -> typeApp i f a
     Pi ty body -> typePi i ty body
     When r l -> assert [isType i $ nf l, isType i $ nf r] Star

typeApp :: Int -> Term Fresh -> Term Fresh -> TyM (Term Fresh)
typeApp i f a = typeTerm i f >>= \case
  Pi ty retTy ->
    assert [checkTerm i a $ nf ty] (instantiate1 (nf a) retTy)
  When r l ->
    assert [checkTerm i a $ nf l] (nf r)
  ty -> do
    argTy <- typeTerm i a
    typeError ty (argTy --> Var (Unbound i))

typePi :: Int -> Term Fresh -> Scope () Term Fresh -> TyM (Term Fresh)
typePi i ty body = assert [ isType i ty
                          , bind i (nf ty) $ isType (i + 1) (unBind i body)]
                   Star

checkTerm :: Int -> Term Fresh -> Term Fresh -> TyM ()
checkTerm i term ty = do
  ty' <- typeTerm i term
  when (nf ty' /= nf ty) $ typeError ty ty'

isType :: Int -> Term Fresh -> TyM ()
isType i ty = checkTerm i ty Star

-- Note 1. This looks inconsistent but I don't think it actually
-- is. Since we don't allow Pi types to be indexed over anything other
-- than *occupants* of Star, we can't get the classic encoding of
-- Russel's paradox. Really I just need a way to politely say "The
-- type of this not interesting" instead of lying with this Star :
-- Star nonsense.
