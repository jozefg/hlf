module Language.HLF.TC.Pre where
import Bound
import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Language.HLF.AST
import Language.HLF.Error
import Language.HLF.TC.Arity
import Language.HLF.TC.Util

isBetaNormal :: Term Fresh -> TyM ()
isBetaNormal = go 0
  where go i t = do
          expr <- addNames t
          local (errorCxt . termExpr .~ Just expr) $
            case t of
             Pi l scope ->
               go i l *> go (i + 1) (instantiate1 (Var $ Unbound i) scope)
             Lam body argTy ->
               go i argTy *> go (i + 1) (instantiate1 (Var $ Unbound i) body)
             When r l -> go i r *> go i l
             Star -> return ()
             Var _ -> return ()
             (Var _ :@: a) :@: b -> go i a *> go i b
             l@(_ :@: _) :@: r -> go i l *> go i r
             Var _ :@: r -> go i r
             ap@(_ :@: _) -> betaError ap

appChain :: Term a -> (Term a, [Term a])
appChain = go []
  where go chain (l :@: r) = go (r : chain) l
        go chain l = (l, chain)

arityM :: Term Fresh -> TyM Int
arityM t = arity 0 <$> view arityMap <*> return t

checkArityBound :: Int -> Term Fresh -> Scope () Term Fresh -> TyM ()
checkArityBound i ty scope = do
  checkArity i ty
  tyAr <- arityM ty
  local (arityMap . at (Unbound i) .~ Just tyAr) $
   checkArity (i + 1) (instantiate1 (Var $ Unbound i) scope)

checkArity :: Int -> Term Fresh -> TyM ()
checkArity j t = do
  a <- arityM t
  when (a /= 0) $ etaError t
  checkEverywhere j t
  where checkEverywhere _ Star = return ()
        checkEverywhere i (Pi ty body) = checkArityBound i ty body
        checkEverywhere i (Lam _ ty) = checkArity i ty
        checkEverywhere i (When r l) = checkArity i l *> checkArity i r
        checkEverywhere _ Var{} = return ()
        checkEverywhere i (_ :@: r) = checkArity i r

isEtaLong :: Term Fresh -> TyM ()
isEtaLong = checkArity 0


preTC :: Term Fresh -> TyM ()
preTC t = isBetaNormal t *> isEtaLong t
