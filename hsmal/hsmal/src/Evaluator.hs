module Evaluator where

import           Control.Monad
import           Syntax

-- Variable Shifting and Substitution
--

-- shiftTerm :: Int -> Term -> Term
-- shiftTerm d = walk 0
--   where walk c (TermVar x n)
--           | x >= c             = TermVar (x+d) (n+d)
--           | otherwise          = TermVar x     (n+d)
--         walk c (TermAbs x ty t1)  = TermAbs x ty (walk (c+1) t1)
--         walk c (TermApp t1 t2) = TermApp (walk c t1) (walk c t2)

-- substTerm :: Int -> Term -> Term -> Term
-- substTerm j s = walk 0
--   where walk c (TermVar x n)
--           | x == j+c           = s
--           | otherwise          = TermVar x n
--         walk c (TermAbs x ty t1)  = TermAbs x ty (walk (c+1) t1)
--         walk c (TermApp t1 t2) = TermApp (walk c t1) (walk c t2)
shiftTerm :: Int -> Term -> Term
shiftTerm d = walk 0
  where walk c (TermIf t1 t2 t3)   = TermIf (walk c t1) (walk c t2) (walk c t3)
        walk c (TermVar x n)
          | x >= c                 = TermVar (x+d) (n+d)
          | otherwise              = TermVar x     (n+d)
        walk c (TermAbs x tyT1 t1) = TermAbs x tyT1 (walk (c+1) t1)
        walk c (TermApp t1 t2)     = TermApp (walk c t1) (walk c t2)
        walk _ t                   = t

substTerm :: Int -> Term -> Term -> Term
substTerm j s = walk 0
  where walk c (TermIf t1 t2 t3)   = TermIf (walk c t1) (walk c t2) (walk c t3)
        walk c (TermVar x n)
          | x == j+c               = s
          | otherwise              = TermVar x n
        walk c (TermAbs x tyT1 t1) = TermAbs x tyT1 (walk (c+1) t1)
        walk c (TermApp t1 t2)     = TermApp (walk c t1) (walk c t2)
        walk c (TermSucc ts)       = TermSucc (walk c ts)
        walk c (TermPred tp)       = TermPred (walk c tp)
        walk _ t1
          | t1 == TermTrue         = t1
          | t1 == TermFalse        = t1
          | t1 == TermZero         = t1
          | t1 == TermUnit         = t1
          | otherwise              = s
substTopTerm :: Term -> Term -> Term
substTopTerm s t = shiftTerm (-1) (substTerm 0 (shiftTerm 1 s) t)

-- Evaluation
--

isValue :: Term -> Bool
isValue (TermAbs _ _ _) = True
isValue TermFalse = True
isValue TermTrue = True
isValue TermZero = True
isValue TermUnit = True
isValue _             = False

eval1 :: Term -> Maybe Term
eval1 (TermApp (TermAbs _ _ t12) v2)
  | isValue v2 = return $ substTopTerm v2 t12
eval1 (TermApp t1 t2)
  | isValue t1 = liftM2 TermApp (return t1) (eval1  t2)
  | otherwise  = liftM2 TermApp (eval1  t1) (return t2)
eval1 TermTrue                         = Nothing
eval1 TermFalse                        = Nothing
eval1 (TermIf TermTrue  t2 _ )         = return t2
eval1 (TermIf TermFalse _  t3)         = return t3
eval1 (TermIf t1        t2 t3)         = liftM (\t1' -> TermIf t1' t2 t3) (eval1 t1)
eval1 TermZero                         = Nothing
eval1 (TermSucc t)                     = liftM TermSucc (eval1 t)
eval1 (TermPred TermZero)              = return TermZero
eval1 (TermPred (TermSucc t))          = return t
eval1 (TermPred t)                     = liftM TermPred (eval1 t)
eval1 (TermIsZero TermZero)            = return TermTrue
eval1 (TermIsZero (TermSucc TermZero)) = return TermFalse
eval1 (TermIsZero t)                   = liftM TermIsZero (eval1 t)
eval1 _ = Nothing


eval :: Term -> Term
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t
