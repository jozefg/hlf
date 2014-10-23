{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.HLF.Error where
import           Control.Lens
import           Control.Monad.Error
import           Control.Monad.Reader
import qualified Data.Text            as T
import           Language.HLF.AST


data WrongThing = TypeError TypeError
                | EnvError EnvError
                | Impossible T.Text
                deriving Show

data TypeError = TypeMismatch {saw, expected :: Term Name}
               | NotBetaNormal (Term Name)
               | NotEtaLong (Term Name)
               deriving Show
data EnvError = UnboundName Name
              | NotAConstr
              deriving Show

data Phase = EnvironmentChecking
           | TypeChecking
           deriving Show

data ErrorContext = ErrorContext { _phase    :: Phase
                                 , _termName :: Maybe Name
                                 , _termExpr :: Maybe (Term Name) }
                  deriving Show
makeLenses ''ErrorContext

data HLFError = HLFError { errProblem :: WrongThing
                         , errContext :: ErrorContext }
              deriving Show

type ErrorM = Either HLFError
type ContextM = ReaderT ErrorContext ErrorM

hlfError :: (Functor m, MonadError HLFError m, MonadReader ErrorContext m)
            => WrongThing -> m a
hlfError wrong = (HLFError wrong `fmap` ask) >>= throwError

impossible :: (Functor m, MonadError HLFError m, MonadReader ErrorContext m)
            => T.Text -> Maybe a -> m a
impossible msg may = case may of
                      Nothing -> hlfError (Impossible msg)
                      Just a -> return a
