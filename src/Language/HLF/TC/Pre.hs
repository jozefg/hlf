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
isBetaNormal t = runGenT (go t)
  where go t = do
          expr <- addNames t
          local (errorCxt . termExpr .~ Just expr) $
            case t of
             (Var _ :@: a) :@: b -> go a *> go b
             l@(_ :@: _) :@: r -> go l *> go r
             Var _ :@: r -> go r
             ap@(_ :@: _) -> lift $ betaError ap
             When r l -> go r *> go l
             Star -> return ()
             Var _ -> return ()
             Pi l scope -> do
               go l
               i <- gen
               go (instantiate1 (Var $ Unbound i) scope)
             Lam body argTy -> do
               go argTy
               i <- gen
               go (instantiate1 (Var $ Unbound i) body)

checkArityBound :: Term Fresh -> Scope () Term Fresh -> GenT Int TyM ()
checkArityBound ty scope = do
  checkArity ty
  tyAr <- view arityMap >>= runReaderT (typeArity ty)
  i <- gen
  local (arityMap . at (Unbound i) .~ Just tyAr) $
    checkArity (instantiate1 (Var $ Unbound i) scope)

checkArity :: Term Fresh -> GenT Int TyM ()
checkArity t = do
  a <- view arityMap >>= runReaderT (termArity t) -- See note 2
  when (a /= 0) $ lift (etaError t a)
  checkEverywhere t -- See note 1
  where checkEverywhere Star = return ()
        checkEverywhere (Pi ty body) = checkArityBound ty body
        checkEverywhere (Lam _ ty) = checkArity ty
        checkEverywhere (When r l) = checkArity l *> checkArity r
        checkEverywhere Var{} = return ()
        checkEverywhere (_ :@: r) = checkArity r

isEtaLong :: Term Fresh -> TyM ()
isEtaLong = runGenT . checkArity


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
--
-- Note 2: There's some subtlility in why we must preserve the Gen
-- monad where in between these calls. Essentially if we don't then
-- termArity could call arityBound which would clober something in the
-- existing ArityMap. Since the new arity we've inserted may not be
-- the same as the old one, catastrophy if that name occurs free in
-- the term whose arity we're checking. This may seem obvious, but I
-- got it wrong the first time round so it gets a note.
