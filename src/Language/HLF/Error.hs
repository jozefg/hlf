{-# LANGUAGE TemplateHaskell #-}
module Language.HLF.Error where
import           Control.Error
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
               deriving Show
data EnvError = UnboundName Name
              deriving Show

data Phase = EnvironmentChecking
           | TypeChecking
           deriving Show

data ErrorContext = ErrorContext { _phase :: Phase
                                 , _termName :: Name
                                 , _termExpr :: Term Name }
                  deriving Show
makeLenses ''ErrorContext

data HLFError = HLFError { errProblem :: WrongThing
                         , errContext :: ErrorContext }
              deriving Show

type ErrorM m = ReaderT ErrorContext (EitherT HLFError m)

hlfError :: Monad m => WrongThing -> ErrorM m a
hlfError wrong = (HLFError wrong `fmap` ask) >>= throwError
