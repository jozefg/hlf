{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.HLF.Env where
import           Bound
import           Control.Applicative
import           Control.Lens         hiding (Context)
import           Control.Monad.Reader
import qualified Data.Foldable        as F
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Traversable     as T
import           Language.HLF.AST
import           Language.HLF.Error

data Definition a = (:=) { defName :: a
                         , defTy   :: Term a }
                  deriving(Show, Functor, F.Foldable, T.Traversable)
infixr 0 :=

data Polarity = Pos | Neg
              deriving (Show)

data TopLevel a = Mode a [Polarity]
                | TypeFamily (Definition a) [Definition a]
                deriving(Show, Functor, F.Foldable, T.Traversable)

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

checkTypeFam :: Name -> [Definition Name] -> ContextM ()
checkTypeFam name constrs = mapM_ checkConstr constrs
  where checkConstr (constrName := term) =
          local (termName .~ Just constrName) $
          local (termExpr .~ Just term) $
            when (not $ endsIn name term) $ hlfError (EnvError NotAConstr)

flattenToplevel :: TopLevel a -> Env a
flattenToplevel (TypeFamily ty constrs) = Env (ty : constrs)
flattenToplevel _ = Env []


lookupName :: Name -> M.Map Name Fresh -> ContextM Fresh
lookupName name nameMap = case M.lookup name nameMap of
  Just i -> return i
  Nothing -> hlfError (EnvError $ UnboundName name)

newtype Env a = Env {unEnv :: [Definition a]}
              deriving(Monoid)

flipAList :: [(a, b)] -> [(b, a)] -- A is short for association, not the article
flipAList = map $ \(a, b) -> (b, a)

topName :: TopLevel a -> Maybe a
topName (TypeFamily (name := _) _)= Just name
topName _ = Nothing

bindTop :: [TopLevel Name] -> ContextM (M.Map Fresh Name, [TopLevel Fresh])
bindTop tops = (,) symMap <$> mapM bindNames tops
  where names = zip (mapMaybe topName tops) (map Free [0..])
        nameMap = M.fromList names
        symMap = M.fromList $ flipAList names
        bindNames = T.traverse (flip lookupName nameMap)

bindProgram :: Program -> ErrorM (M.Map Fresh Name, [TopLevel Fresh])
bindProgram tops = flip runReaderT info $ do
  mapM_ check tops
  bindTop tops
  where info = ErrorContext EnvironmentChecking Nothing Nothing
        check (TypeFamily (name := _) constrs) = checkTypeFam name constrs
        check _ = return ()

processInput :: Program -> ErrorM (M.Map Fresh Name, Context)
processInput tops = fmap (toMap . F.foldMap flattenToplevel) <$> bindProgram tops
  where toMap = M.fromList . map (\(name := term) -> (name, term)) . unEnv
