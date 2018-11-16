import           Control.Monad
data Term = TmVar Int Int
          | TmAbs String Term
          | TmApp Term Term
          deriving (Show)


isval :: Term  -> Bool
isval (TmAbs _ _) = True
isval      _      = False




index2name ctx x = fst (ctx !! x)

pickfreshname ctx t = (t : ctx,t)
termShift :: Int -> Term -> Term
termShift d = walk 0
  where walk :: Int -> Term -> Term
        walk c (TmVar x n)
                | x >= c = TmVar (x + d) (n + d)
                | otherwise = TmVar x (n + d)
        walk c (TmAbs x t1) = TmAbs x (walk (c + 1) t1)
        walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)



termSubst :: Int -> Term -> Term -> Term
termSubst j s = walk 0
  where  walk :: Int -> Term -> Term
         walk c (TmVar x n)
                | x == j + c = termShift c s
                | otherwise = TmVar x n
         walk c (TmAbs x t1) = TmAbs x (walk (c+1) t1)
         walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)


eval1 :: Term -> Maybe Term
eval1 (TmApp (TmAbs _ t12) v2)
  | isval v2 = return $ termSubsetTop v2 t12
eval1 (TmApp t1 t2)
  | isval t1 = liftM2 TmApp (return t1) (eval1  t2)
  | otherwise  = liftM2 TmApp (eval1  t1) (return t2)
eval1 _ = Nothing

termSubsetTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

main = print $ eval1 (TmApp (TmApp (TmAbs "a" (TmVar 0 2)) (TmAbs "b" (TmVar 0 2))) (TmApp (TmAbs "a" (TmVar 0 2)) (TmAbs "b" (TmVar 0 2))))
