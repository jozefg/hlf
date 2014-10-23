{-# LANGUAGE OverloadedStrings #-}
module ShouldError where
import Language.HLF
-- These are a few examples which *should* fail to typecheck. Consider
-- them a canary in our coal mine.

-- All terms need to be eta long. That is, we're only concerned with
-- typechecking canonical terms. Life is better this way, trust me.
notEtaLong :: Program
notEtaLong = Env [ "unit" := Star
                 , "tt" := "unit"
                 , "other" := Star
                 , "ott" := "other"
                 , "notEta" := lam "x" "ott" "unit" :@: "tt" ]


-- No matter the type system, void /= unit.
typeMismatch :: Program
typeMismatch = Env [ "unit" := Star
                   , "tt" := "unit"
                   , "void" := Star
                   , "foo" := "void" --> Star
                   , "bar" := "foo" :@: "tt"]

-- Unbound variables are also a no-no.
unboundVars :: Program
unboundVars = Env ["foo" := "bar"]
