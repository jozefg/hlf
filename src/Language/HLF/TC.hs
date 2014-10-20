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
                         , _errorCxt :: ErrorContext
                         , _context  :: Context }
makeLenses ''TypeInfo

type TyM = ReaderT TypeInfo ErrorM

assert :: [TyM a] -> b -> TyM b
assert as b = F.sequence_ as >> return b

unBind :: Int -> Scope () Term Fresh -> Term Fresh
unBind i sc = instantiate1 (Var (Unbound i)) sc

bind :: Int -> Term Fresh -> TyM a -> TyM a
bind i term act = local (context . at (Unbound i) .~ Just term) act

nameFor :: Fresh -> TyM Name
nameFor i = fromMaybe (fresh2name i) <$> view (nameMap . at i)

lookupVar :: Fresh -> TyM (Term Fresh)
lookupVar i = do
  name <- nameFor i
  term <- view (context . at i)
  magnify errorCxt $
    impossible ("Found unbound symbol " <> name) term


addNames :: Term Fresh -> TyM (Term Name)
addNames = traverse nameFor

typeError :: Term Fresh -> Term Fresh -> TyM a
typeError l r = do
  err <- (TypeMismatch <$> addNames l <*> addNames r)
  magnify errorCxt $ hlfError (TypeError err)

typeTerm :: Int -> Term Fresh -> TyM (Term Fresh)
typeTerm i t = do
  namedT <- addNames t
  local (errorCxt . termExpr .~ Just namedT) $
    case t of
     Star -> return Star
     Var j -> lookupVar j
     Lam body argTy -> bind i argTy $ typeTerm (i + 1) $ unBind i body
     f :@: a ->
       typeTerm i f >>= \case
         Pi ty retTy ->
           assert [checkTerm i a $ nf ty] (instantiate1 (nf a) retTy)
         When r l ->
           assert [checkTerm i a $ nf l] (nf r)
         ty -> do
           argTy <- typeTerm i a
           typeError ty (argTy --> Var (Unbound i))
     Pi ty body ->
       assert [ isType i ty
              , bind i (nf ty) $ isType (i + 1) (unBind i body)]
       Star
     When r l -> assert [isType i $ nf l, isType i $ nf r] Star

checkTerm :: Int -> Term Fresh -> Term Fresh -> TyM ()
checkTerm i term ty = do
  ty' <- typeTerm i term
  when (nf ty' /= nf ty) $ typeError ty ty'


isType :: Int -> Term Fresh -> TyM ()
isType i ty = checkTerm i ty Star

typeProgram :: NameMap -> Context -> ErrorM ()
typeProgram nms cxt = flip runReaderT info
                      . void
                      $ itraverse typeName cxt
  where info = TypeInfo nms (ErrorContext TypeChecking Nothing Nothing) cxt
        typeName i term = do
          n <- nameFor i
          local (errorCxt . termName .~ Just n) $
            typeTerm 0 term
