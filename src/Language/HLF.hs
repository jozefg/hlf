{-# LANGUAGE OverloadedStrings #-}
module Language.HLF ( typeCheck
                    , module Language.HLF.AST
                    , module Language.HLF.Env ) where
import Language.HLF.AST
import Language.HLF.Env
import Language.HLF.TC

typeCheck :: Program -> Bool
typeCheck = maybe False typeProgram . bindEnv

test :: Program
test = Env [ "Nat" := Star
           , "Z"   := Var "Nat"
           , "S"   := piTy (Var "Nat") "" (Var "Nat") ]
