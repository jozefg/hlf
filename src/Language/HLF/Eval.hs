module Language.HLF.Eval where
import Language.HLF.AST

type Env = [Value]

evalInf :: InfTerm -> Env -> Value
evalInf (Ann term _) env = evalUnInf term env
evalInf (Par n) _ = VNeutral (NPar n)
evalInf (Pi l r) env = VPi (evalUnInf l env) $ \v -> evalUnInf r (v : env)
evalInf (Var i) env = env !! i
evalInf Star _ = VStar
evalInf Zero _ = VZero
evalInf Nat _ = VNat
evalInf (Succ e) env = VSucc (evalUnInf e env)
evalInf (l :@: r) env = case evalInf l env of
  VLam f -> f (evalUnInf r env)
  VNeutral n -> VNeutral (NApp n $ evalUnInf r env)
  _ -> error "This is your fault not mine."

evalUnInf :: UnInfTerm -> Env -> Value
evalUnInf (Inf i) env = evalInf i env
evalUnInf (Lam body) env = VLam $ \v -> evalUnInf body (v : env)

quote :: Value -> UnInfTerm
quote = go 0
  where unNeutral _ (NPar n) = Par n
        unNeutral i (NApp l r) = unNeutral i l :@: go i r
        go i (VLam f) = go (i + 1) $ f (VNeutral . NPar . Unquoted $ i)
        go _ VZero = Inf Zero
        go _ VNat = Inf Nat
        go i (VSucc v) = Inf . Succ $ go i v
        go _ VStar = Inf Star
        go i (VNeutral n) = Inf $ unNeutral i n
        go i (VPi t f) = Inf
                         . Pi (go i t)
                         . go (i + 1)
                         $ f (VNeutral . NPar . Unquoted $ i)

substInf :: Int -> InfTerm -> InfTerm -> InfTerm
substInf i t = go
  where go (Ann unInf ann) = substUnInf i t unInf `Ann` substUnInf i t ann
        go Star = Star
        go (Pi ty body) = substUnInf i t ty `Pi` substUnInf i t body
        go (Var j) = if i == j then t else Var j
        go (Par n) = Par n
        go (l :@: r) = substInf i t l :@: substUnInf i t r
        go Zero = Zero
        go Nat = Nat
        go (Succ e) = Succ (substUnInf i t e)

substUnInf :: Int -> InfTerm -> UnInfTerm -> UnInfTerm
substUnInf i t = go
  where go (Inf term) = Inf $ substInf i t term
        go (Lam unInfTerm) = Lam $ substUnInf (i + 1) t unInfTerm
