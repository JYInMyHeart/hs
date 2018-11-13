{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}



data Term a where
   TmFalse :: Term Bool
   TmTrue :: Term Bool
   TmIf ::  Term Bool -> Term a -> Term a -> Term a
   TmZero ::  Term Int
   TmSucc :: Term Int -> Term Int
   TmPred :: Term Int -> Term Int
   TmIsZero :: Term Int -> Term Bool

instance Show a => Show (Term a) where
  show TmFalse    = "false"
  show TmTrue     = "true"
  show TmZero     = "zero"
  show (TmSucc a) = "succ " ++ show a
  show (TmPred a) = "pred" ++ show a


isnumericval :: forall a. Term a -> Bool
isnumericval TmZero     = True
isnumericval (TmSucc a) = isnumericval a
isnumericval _          = False

isval :: Term a -> Bool
isval TmTrue  = True
isval TmFalse = True
isval t       = isnumericval t


eval1 :: Term a -> Term a
eval1 (TmIf TmTrue t2 t3) = t2
eval1 (TmIf TmFalse t2 t3) = t3
eval1 (TmIf t1 t2 t3) = let t1' = eval1 t1 in
  TmIf t1' t2 t3
eval1 (TmSucc t1) = TmSucc t1'
  where t1' = eval1 t1
eval1 (TmPred TmZero) = TmZero
eval1 (TmPred (TmSucc nv1))
  | isnumericval nv1 = nv1
  | otherwise = error "unsupport rules"
eval1 (TmPred t1) = TmPred t1'
  where t1' = eval1 t1
eval1 (TmIsZero TmZero) = TmTrue
eval1 (TmIsZero (TmSucc nv1))
  | isnumericval nv1 = TmFalse
eval1 (TmIsZero t1) = TmIsZero (eval1 t1)
eval1 _ = error "unsupport rules"



eval2 :: Term a -> Term a
eval2 a = case a of
  TmIf t1 t2 t3 -> case eval2 t1 of
    TmTrue  -> eval2 t2
    TmFalse -> eval2 t3
  TmZero -> TmZero
  TmSucc a -> TmSucc (eval2 a)
  TmTrue -> TmTrue
  TmFalse -> TmFalse
  _ -> error "s"

main = print $ eval2 (TmIf TmTrue (TmSucc (TmSucc TmZero)) TmZero)

