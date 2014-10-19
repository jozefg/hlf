module Language.HLF.Eval where
import Bound
import Language.HLF.AST

whnf :: Term a -> Term a
whnf (f :@: a) = case whnf f of
  Lam f' _ -> whnf (instantiate1 a f')
  f' -> f' :@: a
whnf Star = Star
whnf (Pi ty body) = Pi ty body
whnf (When r l) = When r l
whnf (Lam l ty) = Lam l ty
whnf (Var v) = Var v

nfBound :: Scope () Term a -> Scope () Term a
nfBound = toScope . nf . fromScope

nf :: Term a -> Term a
nf (f :@: a) = case nf f of
  Lam f' _ -> nf (instantiate1 (nf a) f')
  f' -> nf f' :@: nf a
nf Star = Star
nf (Pi ty body) = Pi (nf ty) (nfBound body)
nf (When r l) = When (nf r) (nf l)
nf (Lam l ty) = Lam (nfBound l) (nf ty)
nf (Var v) = Var v
