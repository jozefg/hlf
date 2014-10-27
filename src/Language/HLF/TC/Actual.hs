{-# LANGUAGE LambdaCase #-}
module Language.HLF.TC.Actual where
import Bound
import Control.Lens         hiding (Context)
import Control.Monad.Gen
import Control.Monad.Reader
import Language.HLF.AST
import Language.HLF.Error
import Language.HLF.TC.Util

typeTerm :: Term Fresh -> GenT Int TyM (Term Fresh)
typeTerm t = do
  namedT <- addNames t
  local (errorCxt . termExpr .~ Just namedT) $
    case t of
     Star -> return Star -- See note 1
     Var j -> lift $ lookupVar j
     Lam body argTy -> typeLam body argTy
     f :@: a -> typeApp f a
     Pi ty body -> typePi ty body
     When r l -> assert [isType l, isType r] Star

typeLam :: Scope () Term Fresh -> Term Fresh -> GenT Int TyM (Term Fresh)
typeLam body argTy = do
  i <- gen
  resultTy <- bind i argTy $ typeTerm (unBind i body)
  return $ Pi argTy (abstract1 (Unbound i) resultTy)

typeApp :: Term Fresh -> Term Fresh -> GenT Int TyM (Term Fresh)
typeApp f a = typeTerm f >>= \case
  Pi ty retTy ->
    assert [checkTerm a ty] (instantiate1 a retTy)
  When r l -> assert [checkTerm a l] r
  ty -> do
    i <- gen
    argTy <- typeTerm a
    lift $ typeError ty (Pi argTy (abstract (const Nothing) $ Var (Unbound i)))

typePi :: Term Fresh -> Scope () Term Fresh -> GenT Int TyM (Term Fresh)
typePi ty body = do
  i <- gen
  assert [ isType ty
         , bind i ty $ isType (unBind i body)]
    Star

checkTerm :: Term Fresh -> Term Fresh -> GenT Int TyM ()
checkTerm term ty = do
  expr <- lift $ addNames term
  local (errorCxt . termExpr .~ Just expr) $ do
    ty' <- typeTerm term
    when (ty' /= ty) . lift $ typeError ty ty'

isType :: Term Fresh -> GenT Int TyM ()
isType ty = checkTerm ty Star

check :: Term Fresh -> TyM ()
check = runGenT . isType

-- Note 1. This looks inconsistent but I don't think it actually
-- is. Since we don't allow Pi types to be indexed over anything other
-- than *occupants* of Star, we can't get the classic encoding of
-- Russel's paradox. Really I just need a way to politely say "The
-- type of this not interesting" instead of lying with this Star :
-- Star nonsense.
