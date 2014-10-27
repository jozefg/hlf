{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.HLF.Env where
import           Bound
import           Control.Applicative
import           Control.Lens         hiding (Context)
import           Control.Monad.Reader
import qualified Data.Map             as M
import qualified Data.Foldable        as F
import           Data.Monoid
import           Language.HLF.AST
import           Language.HLF.Error

data Definition a = (:=) { defName :: Name
                         , defTy   :: Term Name }
                  deriving(Show)
infixr 0 :=

data Polarity = Pos | Neg

data TopLevel a = Mode a [Polarity]
                | TypeFamily (Definition a) [Definition a]

type Program = [TopLevel Name]

endsIn :: Eq a => a -> Term a -> Bool
endsIn name = go . fmap (== name)
  where go t = case t of
          Var isName -> isName
          When r _ -> go r
          Var isName :@: _ -> isName
          f@(_ :@: _) :@: _ -> go f
          Pi _ body -> go (instantiate1 (Var False) body)
          _ -> False

checkTypeFam :: Name -> [Definition a] -> ContextM ()
checkTypeFam name constrs = mapM_ checkConstr constrs
  where checkConstr (constrName := term) =
          local (termName .~ Just constrName) $
          local (termExpr .~ Just term) $
            when (not $ endsIn name term) $ hlfError (EnvError NotAConstr)

flattenTypeFam :: TopLevel a -> Env a
flattenTypeFam (TypeFamily ty constrs) = Env (ty : constrs)


lookupName :: Name -> M.Map Name Fresh -> ContextM Fresh
lookupName name nameMap = case M.lookup name nameMap of
  Just i -> return i
  Nothing -> hlfError (EnvError $ UnboundName name)

newtype Env a = Env {unEnv :: [Definition a]}
              deriving(Monoid)

flipAList :: [(a, b)] -> [(b, a)]
flipAList = map $ \(a, b) -> (b, a)

bindEnv :: Env Name -> ContextM (M.Map Fresh Name, Context)
bindEnv (Env env) = (,) symMap <$> foldr bindTy (return M.empty) env
  where names = zip (map defName env) (map Free [0..])
        nameMap = M.fromList names
        symMap = M.fromList $ flipAList names
        bindTy (name := ty) tyMap =
          local ((termName .~ Just name)
                 . (termExpr .~ Just ty)) $
          M.insert <$> lookupName name nameMap
                   <*> traverse (flip lookupName nameMap) ty
                   <*> tyMap

processInput :: Program -> ErrorM (M.Map Fresh Name, Context)
processInput fams = flip runReaderT info $ do
  mapM_ check fams
  bindEnv $ F.foldMap flattenTypeFam fams
  where info = ErrorContext EnvironmentChecking Nothing Nothing
        check (TypeFamily (name := _) constrs) = checkTypeFam name constrs
        check _ = return ()
