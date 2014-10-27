{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.HLF.Env where
import           Bound
import           Control.Applicative
import           Control.Lens         hiding (Context)
import           Control.Monad.Reader
import qualified Data.Foldable        as F
import qualified Data.Map             as M
import           Data.Monoid
import qualified Data.Set             as S
import qualified Data.Traversable     as T
import           Language.HLF.AST
import           Language.HLF.Error

endsIn :: Eq a => a -> Term a -> Bool
endsIn name = go . fmap (== name)
  where go t = case t of
          Var isName -> isName
          When r _ -> go r
          Var isName :@: _ -> isName
          f@(_ :@: _) :@: _ -> go f
          Pi _ body -> go (instantiate1 (Var False) body)
          _ -> False

checkTypeFam :: Name -> [Definition Name] -> ContextM ()
checkTypeFam name constrs = mapM_ checkConstr constrs
  where checkConstr (constrName := term) =
          local (termName .~ Just constrName) $
          local (termExpr .~ Just term) $
            when (not $ endsIn name term) $ hlfError (EnvError NotAConstr)

lookupName :: Name -> M.Map Name Fresh -> ContextM Fresh
lookupName name nameMap = case M.lookup name nameMap of
  Just i -> return i
  Nothing -> hlfError (EnvError $ UnboundName name)

bindTop :: [TopLevel Name] -> ContextM (M.Map Fresh Name, [TopLevel Fresh])
bindTop tops = (,) symMap <$> mapM bindNames tops
  where names = F.toList $ F.foldMap (F.foldMap S.singleton) tops
        nameMap = M.fromList $ zip names (map Free [0..])
        symMap = M.fromList $ zip (map Free [0..]) names
        bindNames = T.traverse (flip lookupName nameMap)

bindProgram :: Program -> ErrorM (M.Map Fresh Name, [TopLevel Fresh])
bindProgram tops = flip runReaderT info $ do
  mapM_ check tops
  bindTop tops
  where info = ErrorContext EnvironmentChecking Nothing Nothing
        check (TypeFamily (name := _) constrs) = checkTypeFam name constrs
        check _ = return ()

flattenToplevel :: TopLevel a -> [Definition a]
flattenToplevel (TypeFamily ty constrs) = ty : constrs
flattenToplevel _ = []

processInput :: Program -> ErrorM (M.Map Fresh Name, Context)
processInput tops = fmap (toCxt . F.foldMap flattenToplevel)
                    <$> bindProgram tops
  where toCxt = map (\(name := term) -> (name, term))
