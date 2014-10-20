{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Language.HLF.TC where
import           Bound
import           Control.Applicative
import           Control.Lens         hiding (Context)
import           Control.Monad        (guard)
import           Control.Monad.Reader
import qualified Data.Foldable        as F
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text            as Te
import qualified Data.Traversable     as T
import           Language.HLF.AST
import           Language.HLF.Error
import           Language.HLF.Eval

type NameMap = M.Map Fresh Name
type TyM = ErrorM (Reader NameMap)

assert :: [TyM a] -> b -> TyM b
assert as b = F.sequence_ as >> return b

unBind :: Int -> Scope () Term Fresh -> Term Fresh
unBind i sc = instantiate1 (Var (Unbound i)) sc

bind :: Int -> Term Fresh -> Context -> Context
bind = M.insert . Unbound

lookupVar :: Fresh -> Context -> TyM (Term Fresh)
lookupVar i cxt = do
  name <- lift $ asks (M.! i)
  case M.lookup i cxt of
   Just ty -> return ty
   Nothing -> hlfError . Impossible $ "Found unbound name " <> name

addNames :: Term Fresh -> TyM (Term Name)
addNames = traverse $ \i ->  lift $ asks (M.! i)

typeError :: Term Fresh -> Term Fresh -> TyM a
typeError l r =
  (TypeMismatch <$> addNames l <*> addNames r) >>= hlfError . TypeError

typeTerm :: Int -> Context -> Term Fresh -> TyM (Term Fresh)
typeTerm i cxt t = do
  namedT <- addNames t
  local (set termExpr namedT) $
    case t of
    Star -> return Star
    Var j -> lookupVar j cxt
    Lam body argTy -> typeTerm (i + 1) (bind i argTy cxt) $ unBind i body
    f :@: a ->
      typeTerm i cxt f >>= \case
        Pi ty retTy ->
          assert [checkTerm i cxt a $ nf ty] (instantiate1 (nf a) retTy)
        When r l ->
          assert [checkTerm i cxt a $ nf l] (nf r)
        ty -> do
          argTy <- typeTerm i cxt a
          typeError ty (argTy --> Var (Unbound i))
    Pi ty body ->
      assert [ isType i cxt ty
             , isType (i + 1) (bind i (nf ty) cxt) (unBind i body)]
      Star
    When r l -> assert [isType i cxt $ nf l, isType i cxt $ nf r]
                Star

checkTerm :: Int -> Context -> Term Fresh -> Term Fresh -> ErrorM (Reader NameMap) ()
checkTerm i cxt term ty = do
  ty' <- typeTerm i cxt term
  when (nf ty' /= nf ty) $ typeError ty ty'


isType :: Int -> Context -> Term Fresh -> TyM ()
isType i cxt ty = checkTerm i cxt ty Star

typeProgram :: NameMap -> Context -> Bool
typeProgram nm cxt = undefined (T.traverse (typeTerm 0 cxt) cxt)
