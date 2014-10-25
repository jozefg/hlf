{-# LANGUAGE OverloadedStrings #-}
module EtaShorthand where
import Language.HLF

program :: Program
program = [ TypeFamily ("Nat" := Star)
            ["z" := "Nat" , "s" := "Nat" --> "Nat"]
          , TypeFamily ("bar" := "Nat" --> Star)
            ["test" := "bar":@:("s":@:"z")]
          , TypeFamily ("foo" := ("Nat" --> "Nat") --> Star)
            ["good" := "foo" :@: (lam "x" ("s" :@: "x") "Nat")
            ,"bad" := "foo" :@: "s"]]
