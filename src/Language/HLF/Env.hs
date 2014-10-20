{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.HLF.Env where
import           Control.Applicative
import           Control.Lens         hiding (Context)
import           Control.Monad.Reader
import qualified Data.Map             as M
import           Data.Monoid
import qualified Data.Text            as T
import           Data.Traversable
import           Language.HLF.AST
import           Language.HLF.Error

data Definition a = (:=) { defName :: Name
                         , defTy   :: Term Name }
                  deriving(Show)
infixr 0 :=

newtype Env a = Env {unEnv :: [Definition a]}
              deriving(Monoid)
type Program = Env Name

lookupName :: Name -> M.Map Name Fresh -> ContextM Fresh
lookupName name map = case M.lookup name map of
  Just i -> return i
  Nothing -> hlfError (EnvError $ UnboundName name)

flipAList :: [(a, b)] -> [(b, a)]
flipAList = map $ \(a, b) -> (b, a)

bindEnv :: Program -> ContextM (M.Map Fresh Name, Context)
bindEnv (Env env) = (M.fromList $ flipAList names,)
                    <$> foldr bindTy (return M.empty) env
  where names = zip (map defName env) (map Free [0..])
        nameMap = M.fromList names
        bindTy (name := ty) tyMap =
          local (set termName name . set termExpr ty) $
          M.insert <$> lookupName name nameMap
                   <*> traverse (flip lookupName nameMap) ty
                   <*> tyMap
