module Language.HLF.AST where

data Name = Const String
          | Bound Int
          | Unquoted Int
          deriving (Eq, Ord, Show)

type Type = Value

-- | Terms which we can infer types for.
data InfTerm = Ann UnInfTerm UnInfTerm
             | Star -- Kind of types
             | Pi UnInfTerm UnInfTerm
             | Var Int -- De Bruijn sadness
             | Par Name -- Free variables, name from simple easy!
             | InfTerm :@: UnInfTerm
             | Zero
             | Succ UnInfTerm
             | Nat
             deriving (Eq, Show)


data UnInfTerm = Inf InfTerm
               | Lam UnInfTerm
               deriving (Eq, Show)

data Neutral = NPar Name
             | NApp Neutral Value

data Value = VLam (Value -> Value)
           | VStar
           | VNat
           | VPi Value (Value -> Value)
           | VNeutral Neutral
           | VZero
           | VSucc Value
