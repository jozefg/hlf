module Language.HLF.Error where
import qualified Data.Text        as T
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

data ErrorContext = ErrorContext { phase :: Phase
                                 , termName :: Name
                                 , termExpr :: Term Name }
                  deriving Show

data HLFError = HLFError { errProblem :: WrongThing
                         , errContext :: ErrorContext
                         , errNotes   :: Maybe T.Text }
              deriving Show
