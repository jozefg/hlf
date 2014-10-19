{-# LANGUAGE OverloadedStrings #-}
module Nats where
import Language.HLF

program :: Program
program = Env [ "Nat" := Star
              , "S"   := piTy "Nat" "" "Nat"
              , "Z"   := "Nat"
              , "plus" :=
                piMany [("Nat", ""), ("Nat", ""), ("Nat", "")] Star
              , "plusZ" := piTy "Nat" "N" ("plus" :@: "Z" :@: "N" :@: "N")
              , "plusS" :=
                piMany [("Nat", "N"), ("Nat", "M"), ("Nat", "O")]
                ("plus":@:"N":@:"M":@:"O"
                 <-- "plus":@:("S":@:"N"):@:"M":@:("S":@:"O"))]
