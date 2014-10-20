{-# LANGUAGE OverloadedStrings, LambdaCase, TemplateHaskell #-}
module Language.HLF.TC where
import           Bound
import           Control.Applicative
import           Control.Lens         hiding (Context)
import           Control.Monad.Reader
import qualified Data.Foldable        as F
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Monoid
import           Language.HLF.AST
import           Language.HLF.Error
import           Language.HLF.Eval

type NameMap = M.Map Fresh Name
data TypeInfo = TypeInfo { _nameMap  :: NameMap
                         , _errorCxt :: ErrorContext }
makeLenses ''TypeInfo

type TyM = ReaderT TypeInfo ErrorM

assert :: [TyM a] -> b -> TyM b
assert as b = F.sequence_ as >> return b

unBind :: Int -> Scope () Term Fresh -> Term Fresh
unBind i sc = instantiate1 (Var (Unbound i)) sc

bind :: Int -> Term Fresh -> Context -> Context
bind = M.insert . Unbound

nameFor :: Fresh -> TyM Name
nameFor i = fromMaybe (fresh2name i) <$> view (nameMap . at i)

lookupVar :: Fresh -> Context -> TyM (Term Fresh)
lookupVar i cxt = do
  name <- nameFor i
  magnify errorCxt $
    impossible ("Found unbound symbol " <> name) (M.lookup i cxt)

addNames :: Term Fresh -> TyM (Term Name)
addNames = traverse nameFor

typeError :: Term Fresh -> Term Fresh -> TyM a
typeError l r = do
  err <- (TypeMismatch <$> addNames l <*> addNames r)
  magnify errorCxt $ hlfError (TypeError err)

typeTerm :: Int -> Context -> Term Fresh -> TyM (Term Fresh)
typeTerm i cxt t = do
  namedT <- addNames t
  local (errorCxt . termExpr .~ Just namedT) $
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

checkTerm :: Int -> Context -> Term Fresh -> Term Fresh -> TyM ()
checkTerm i cxt term ty = do
  ty' <- typeTerm i cxt term
  when (nf ty' /= nf ty) $ typeError ty ty'


isType :: Int -> Context -> Term Fresh -> TyM ()
isType i cxt ty = checkTerm i cxt ty Star

typeProgram :: NameMap -> Context -> ErrorM ()
typeProgram nms cxt = flip runReaderT info
                      . void
                      $ itraverse typeName cxt
  where info = TypeInfo nms $ ErrorContext TypeChecking Nothing Nothing
        typeName i term = do
          n <- nameFor i
          local (errorCxt . termName .~ Just n) $
            typeTerm 0 cxt term
