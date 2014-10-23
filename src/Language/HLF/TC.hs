module Language.HLF.TC where
import Control.Lens           hiding (Context)
import Control.Monad.Reader
import Language.HLF.AST
import Language.HLF.Error
import Language.HLF.TC.Actual
import Language.HLF.TC.Pre
import Language.HLF.TC.Util

typeProgram :: NameMap -> Context -> ErrorM ()
typeProgram nms cxt = flip runReaderT info
                      . void
                      $ itraverse typeName cxt
  where info = TypeInfo nms (ErrorContext TypeChecking Nothing Nothing) cxt
        typeName i term = do
          n <- nameFor i
          local (errorCxt . termName .~ Just n) $ do
            preTC term
            typeTerm 0 term
