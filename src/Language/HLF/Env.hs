{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.HLF.Env where
import           Control.Applicative
import qualified Data.Map            as M
import           Data.Monoid
import qualified Data.Text           as T
import           Data.Traversable
import           Language.HLF.AST

type Name = T.Text
data Definition a = (:=) { defName :: Name
                         , defTy   :: Term Name }
                  deriving(Show)

newtype Env a = Env {unEnv :: [Definition a]}
              deriving(Monoid)
type Program = Env Name

bindEnv :: Program -> Maybe Context
bindEnv (Env env) = foldr bindTy (Just M.empty) env
  where nameMap = M.fromList $ zip (map defName env) (map Free [0..])
        bindTy (name := ty) tyMap =
          let name' = nameMap M.! name
              ty' = traverse (flip M.lookup nameMap) ty
          in M.insert name' <$> ty' <*> tyMap
