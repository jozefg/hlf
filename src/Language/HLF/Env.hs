{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.HLF.Env where
import qualified Data.Map         as M
import           Data.Monoid
import qualified Data.Text        as T
import           Language.HLF.AST

type Name = T.Text
data Definition a = (:=) { defName :: Name
                         , defTy   :: Term Name }
                  deriving(Show)

newtype Env a = Env {unEnv :: [Definition a]}
              deriving(Monoid)
type Program = Env Name

bindEnv :: Program -> (M.Map Name Int, M.Map Int (Term Int))
bindEnv (Env env) = (nameMap, foldr bindTy M.empty env)
  where nameMap = M.fromList $ zip (map defName env) [0..]
        bindTy (name := ty) tyMap =
          let name' = nameMap M.! name
              ty'   = fmap (nameMap M.!) ty
          in M.insert name' ty' tyMap
