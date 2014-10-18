module Language.HLF.TC where
import           Control.Monad
import qualified Data.Map          as M
import           Data.Maybe
import           Language.HLF.AST
import           Language.HLF.Eval


type Context = M.Map Name Type
type FreshCounter = Int

typeInf :: FreshCounter -> Context -> InfTerm -> Maybe Type
typeInf i cxt = go
  where go Star = Just VStar
        go Nat = Just VStar
        go Zero = Just VNat
        go (Succ e) = typeUnInf i cxt e VNat >> return VNat
        go (Var j) = M.lookup (Bound j) cxt
        go (Par n) = M.lookup n cxt
        go (Ann un ty) = do
          let v = evalUnInf ty []
          typeUnInf i cxt ty VStar
          typeUnInf i cxt un v
          return v
        go (f :@: a) =
          case typeInf i cxt f of
           Just (VPi ty retTy) ->
             typeUnInf i cxt a ty >> return (retTy $ evalUnInf a [])
           _ -> error "This is your fault not mine."
        go (Pi ty body) = do
          let val = evalUnInf ty []
              cxt' = M.insert (Bound i) val cxt
              body' = substUnInf 0 (Par . Bound $ i) body
          typeUnInf i cxt ty VStar
          typeUnInf (i + 1) cxt' body' VStar
          return VStar

typeUnInf :: FreshCounter -> Context -> UnInfTerm -> Type -> Maybe ()
typeUnInf i cxt (Lam unInf) (VPi ty retTy) =
  let cxt' = M.insert (Bound i) ty cxt
      unInf' = substUnInf i (Par $ Bound i) unInf
  in typeUnInf (i + 1) cxt' unInf' (retTy . VNeutral . NPar . Bound $ i)
typeUnInf i cxt (Inf term) t = do
  t' <- typeInf i cxt term
  guard (quote t' == quote t)
typeUnInf _ _ _ _ = Nothing

hasType :: UnInfTerm -> Type -> Bool
hasType term ty = isJust $ typeUnInf 0 M.empty term ty

typeOf :: InfTerm -> Maybe Type
typeOf = typeInf 0 M.empty
