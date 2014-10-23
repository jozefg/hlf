module Language.HLF.TC.Pre where
import Bound
import Control.Applicative
import Control.Lens
import Control.Monad.Reader
import Language.HLF.AST
import Language.HLF.Error
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

preTC :: Term Fresh -> TyM ()
preTC = isBetaNormal
