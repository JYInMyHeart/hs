{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}
data Nat = Zero | Succ Nat deriving Show
data List a = Nil | Cons a (List a) deriving Show

one = Succ Zero
two = Succ one
listOne :: List  Nat
listOne = Cons Zero Nil
listTwo = Cons (Succ Zero) listOne

listIndex :: Nat -> List a -> a
listIndex Zero (Cons x _)      = x
listIndex (Succ n) (Cons _ xs) = listIndex n xs

data Vec :: * -> Nat -> * where
  VNil :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

instance Show a => Show (Vec a n) where
  show VNil        = "nil"
  show (VCons a b) = "cons " ++ show a ++ " " ++ show b

vecOne :: Vec Nat (Succ Zero)
vecOne = VCons Zero VNil
vecTwo :: Vec Nat (Succ (Succ Zero))
vecTwo = VCons Zero $ VCons (Succ Zero) VNil



type family (a :: Nat) :< (b :: Nat) where
  m :< 'Zero = 'False
  Zero :< 'Succ n = 'True
  ('Succ m) :< ('Succ n) = m :< n

data SNat  a where
  SZero :: SNat 'Zero
  SSucc ::  SNat a -> SNat ('Succ a)


-- data SNat :: Nat → * where
--   SZero :: SNat ’Zero
--   SSucc :: ∀ (n ::Nat). SNat n → SNat (’Succ n)
vecIndex :: ((a :< b) ~ True) => SNat a -> Vec x b -> x
vecIndex SZero (VCons x _)      = x
vecIndex (SSucc n) (VCons _ xs) = vecIndex n xs

main = print $ vecIndex ( SSucc SZero) vecTwo
