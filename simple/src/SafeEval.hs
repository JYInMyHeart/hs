import Control.Monad
data Exp = Lit Integer
    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Div Exp Exp


safeEval :: Exp -> Maybe Integer
safeEval (Lit n) = Just n
safeEval (Add e1 e2) = 
    safeEval e1 >>= \n1 ->
    safeEval e2 >>= \n2 ->
        return (n1 + n2)
safeEval (Sub e1 e2) = 
    safeEval e1 >>= \n1 ->
    safeEval e2 >>= \n2 ->
        return (n1 - n2)
safeEval (Mul e1 e2) = 
    safeEval e1 >>= \n1 ->
    safeEval e2 >>= \n2 ->
        return (n1 * n2)
safeEval (Div e1 e2) = 
    safeEval e1 >>= \n1 ->
    safeEval e2 >>= \n2 ->
        return (n1 `div` n2)

safeDiv :: Double -> Double -> Maybe Double
safeDiv a 0 = Nothing
safeDiv a b = Just (a / b)
main = print $ foldM safeDiv 100 [1,2,3,4]
