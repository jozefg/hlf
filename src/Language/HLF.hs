module Language.HLF ( typeCheck
                    , module Language.HLF.AST
                    , module Language.HLF.Env
                    , module Language.HLF.Error) where
import Language.HLF.AST
import Language.HLF.Env
import Language.HLF.Error
import Language.HLF.TC
import Control.Monad

typeCheck :: Program -> Either HLFError ()
typeCheck = bindEnv >=> uncurry typeProgram
