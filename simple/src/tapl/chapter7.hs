{-# LANGUAGE GADTs #-}

data Term a where
  TmVar :: Int -> Int -> Term Int
  TmAbs :: Term a -> String -> Term a
  TmApp :: Term a -> Term a -> Term a
type Context = [(String,NameBind)]
type NameBind = String
instance Show a => Show (Term a) where
  show (TmVar t1 t2) = "TmVar " ++ show t1
  show (TmAbs s t)   = "TmAbs " ++ show s ++ show t
  show (TmApp s t)   = "TmApp " ++ show s ++ show t


printtm ctx t = case t of
  TmAbs x t1 -> let (ctx',x') = pickfreshname ctx x in
    print $ "(lambda " ++ x' ++ "." ++ (printtm ctx' t1) ++ ")"
  TmApp t1 t2 ->
    print $ "(" ++ printtm ctx t1 ++ " " ++ printtm ctx t2++ ")"
  TmVar x n ->
    if ctxlength ctx == n then print $ index2name ctx x
    else print $ "bad index"

ctxlength = length

index2name ctx x = fst (ctx !! x)

pickfreshname ctx t = (ctx,t)
