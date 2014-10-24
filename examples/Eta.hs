{-# LANGUAGE OverloadedStrings #-}
module EtaShorthand where
import Language.HLF

program :: Program
program = Env [ "Nat" := Star
              , "z" := "Nat"
              , "s" := "Nat" --> "Nat"
              , "bar" := "Nat" --> Star
              , "test" := "bar":@:("s":@:"z")
              , "foo" := ("Nat" --> "Nat") --> Star
              , "good" := "foo" :@: (lam "x" ("s" :@: "x") "Nat")
              , "bad" := "foo" :@: "s"]
