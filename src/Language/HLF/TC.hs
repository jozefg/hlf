module Language.HLF.TC where
import qualified Data.Map as M
import           Language.HLF.AST

type Context = M.Map Name Type
type Env = [Value]

evalInf :: InfTerm -> Env -> Value
evalInf (Ann term _) env = evalUnInf term env
evalInf (Par n) _ = VNeutral (NPar n)
evalInf (Pi l r) env = VPi (evalUnInf l env) $ \v -> evalUnInf r (v : env)
evalInf (Var i) env = env !! i
evalInf (l :@: r) env = case evalInf l env of
  VLam f -> f (evalUnInf r env)
  VNeutral n -> VNeutral (NApp n $ evalUnInf r env)
  _ -> error "This is your fault not mine."
evalInf Star _ = VStar

evalUnInf :: UnInfTerm -> Env -> Value
evalUnInf (Inf i) env = evalInf i env
evalUnInf (Lam body) env = VLam $ \v -> evalUnInf body (v : env)

quote :: Value -> UnInfTerm
quote = go 0
  where go i (VLam f) = go (i + 1) $ f (VNeutral . NPar . Unquoted $ i)
        go i VStar = Inf Star
        go i (VNeutral n) = Inf $ unNeutral i n
        go i (VPi t f) = Inf
                         . Pi (go i t)
                         . go (i + 1)
                         $ f (VNeutral . NPar . Unquoted $ i)
        unNeutral i (NPar n)   = Par n
        unNeutral i (NApp l r) = unNeutral i l :@: go i r
