{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.HLF.TC.Util where
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
typeError expected saw = do
  err <- (TypeMismatch <$> addNames saw <*> addNames expected)
  magnify errorCxt $ hlfError (TypeError err)

betaError :: Term Fresh -> TyM a
betaError t = do
  error <- NotBetaNormal <$> addNames t
  magnify errorCxt $ hlfError (TypeError error)
