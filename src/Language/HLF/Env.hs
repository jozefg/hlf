{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.HLF.Env where
import           Bound
import           Control.Applicative
import           Control.Lens         hiding (Context)
import           Control.Monad.Reader
import qualified Data.Map             as M
import           Data.Monoid
import           Language.HLF.AST
import           Language.HLF.Error

data Definition a = (:=) { defName :: Name
                         , defTy   :: Term Name }
                  deriving(Show)
infixr 0 :=

data TypeFamily a = TypeFamily { tyFam   :: Definition a
                               , constrs :: [Definition a]}


lookupName :: Name -> M.Map Name Fresh -> ContextM Fresh
lookupName name nameMap = case M.lookup name nameMap of
  Just i -> return i
  Nothing -> hlfError (EnvError $ UnboundName name)

newtype Env a = Env {unEnv :: [Definition a]}
              deriving(Monoid)
type Program = Env Name

flipAList :: [(a, b)] -> [(b, a)]
flipAList = map $ \(a, b) -> (b, a)

bindEnv :: Program -> ErrorM (M.Map Fresh Name, Context)
bindEnv (Env env) = flip runReaderT info
                    $ (,) symMap <$> foldr bindTy (return M.empty) env
  where info = ErrorContext EnvironmentChecking Nothing Nothing
        names = zip (map defName env) (map Free [0..])
        nameMap = M.fromList names
        symMap = M.fromList $ flipAList names
        bindTy (name := ty) tyMap =
          local ((termName .~ Just name)
                 . (termExpr .~ Just ty)) $
          M.insert <$> lookupName name nameMap
                   <*> traverse (flip lookupName nameMap) ty
                   <*> tyMap
