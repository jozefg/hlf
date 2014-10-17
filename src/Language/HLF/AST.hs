module Language.HLF.AST where

data Name = Const String
          | Bound Int
          | Unquoted Int

type Type = InfTerm

-- | Terms which we can infer types for.
data InfTerm = Ann UnInfTerm Type
             | Star -- Kind of types
             | Pi InfTerm InfTerm
             | Var Int -- De Bruijn sadness
             | Par Name -- Free variables, name from simple easy!
             | InfTerm :@: UnInfTerm


data UnInfTerm = Inf InfTerm
               | Lam UnInfTerm

data Neutral = NPar Name
             | NApp Neutral Value

data Value = VLam (Value -> Value)
           | VStar
           | VPi Value (Value -> Value)
           | VNeutral Neutral
