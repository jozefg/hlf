module Language.HLF ( typeCheck
                    , module Language.HLF.AST
                    , module Language.HLF.Env
                    , module Language.HLF.Error) where
import Bound
import Language.HLF.AST
import Language.HLF.Env
import Language.HLF.Error
import Language.HLF.TC
import Control.Monad

typeCheck :: Program -> Either HLFError ()
typeCheck = processInput >=> uncurry typeProgram

lam :: Eq a => a -> Term a -> Term a -> Term a
lam a f = Lam (abstract1 a f)

piTy :: Eq a => Term a -> a -> Term a -> Term a
piTy ty a f = Pi ty (abstract1 a f)

piMany :: Eq a => [(Term a, a)] -> Term a -> Term a
piMany vars body = foldr (uncurry piTy) body vars

(<--) :: Term a -> Term a -> Term a
(<--) = When
infixl 0 <--

(-->) :: Term a -> Term a -> Term a
l --> r = Pi l (abstract (const Nothing) r)
infixr 0 -->
