module Language.HLF.TC.Pre where
import Bound
import Control.Applicative
import Control.Lens
import Control.Monad.Gen
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

arityM :: Term Fresh -> TyM Int
arityM t = runGenT (termArity t) <$> view arityMap

tyArityM :: Term Fresh -> TyM Int
tyArityM t = runGenT (typeArity t) <$> view arityMap

checkArityBound :: Int -> Term Fresh -> Scope () Term Fresh -> TyM ()
checkArityBound i ty scope = do
  checkArity i ty
  tyAr <- tyArityM ty
  local (arityMap . at (Unbound i) .~ Just tyAr) $
   checkArity (i + 1) (instantiate1 (Var $ Unbound i) scope)

checkArity :: Int -> Term Fresh -> TyM ()
checkArity j t = do
  a <- arityM t
  when (a /= 0) $ etaError t a
  checkEverywhere j t -- See note 1
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

-- Note 1: checkEverywhere makes me unhappy. Basically when we're
-- looking at some expression and trying assert that it's eta-long, we
-- need to ensure to things.
--
--  1. The top level structure is eta-long
--  2. Each "sub node" is eta long
--
-- It's quite easy to check that the toplevel structure is
-- eta-long. All we need to is say "hey termArity, are you zero?" and
-- complain loudly if this is not so. Checking each subnode is
-- tricky. Essentially we need to poke around each subexpression and
-- do what we did at the toplevel, however, we can do this naivly. If
-- we just look in at each subexpression and call termArity we'll find
-- that this complains about something like `l :@: r :@: z` since `l
-- :@: r` isn't eta long but is a subexpression. Instead we look into
-- the subexpression that aren't taken into account for termArity and
-- poke at those.
