{-# LANGUAGE OverloadedStrings #-}
module LC where
import Language.HLF

-- We can encode typing judgements for STLC using HOAS and black
-- magic. I justify this with the quote "I can, and therefore I
-- should". At least that's how I think it went...
program :: Program
program = Env [ "st"  := Star
              , "ap"  := "st" --> "st" --> "st"
              , "lam" := ("st" --> "st") --> "st"
              , "tt"  := "st"
              , "ty"  := Star
              , "arr" := "ty" --> "ty" --> "ty"
              , "unit" := "ty"
              , "of" := "st" --> "ty" --> Star
              , "of/tt" := "of" :@: "tt" :@: "unit"
              , "of/ap" :=
                piMany [("st", "f"), ("st", "a"), ("ty", "l"), ("ty", "r")]
                ("of" :@: ("ap":@:"f":@:"a") :@: "r"
                 <-- "of" :@: "f" :@: ("arr":@:"l":@:"r")
                 <-- "of" :@: "a" :@: "l")
              , "of/lam" :=
                piMany [("st", "f"), ("ty", "l"), ("ty", "r")]
                ("of" :@: "f" :@: ("arr":@:"l":@:"r")
                 <-- (piTy "st" "a" $
                      "of" :@: "a" :@: "l"
                      --> "of" :@: ("ap":@:"f":@:"a") :@: "r"))]
