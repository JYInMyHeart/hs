{-# LANGUAGE NoImplicitPrelude, GADTs , DataKinds, TypeFamilies, TypeOperators, RankNTypes, DeriveFunctor,PolyKinds #-}

data Vec a n where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- promoted to type level by data kinds
data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat Zero
  SSucc :: SNat a -> SNat (Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool
type instance m :< Zero = False
type instance Zero :< Succ n = True
type instance (Succ m) :< (Succ n) = m :< n


type family (Add (a :: Nat) (b :: Nat)) :: Nat
type instance Add Zero m = m
type instance Add (Succ n) m = Succ (Add n m)
-- to be defined

map :: (a -> b) -> Vec a n -> Vec b n
map f VNil = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index SZero (VCons x _)      = x
index (SSucc n) (VCons _ xs) = index n xs

replicate :: s -> SNat a -> Vec s a
replicate _ SZero = VNil 
replicate a (SSucc n) = VCons a (replicate a n)




-- Both vectors must be of equal length
zipWith :: ((a :=: b) ~ True) => Vec s a -> Vec s b -> Vec (s,s) a
zipWith = undefined

(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
a ++ b = undefined 

-- The semantics should match that of take for normal lists.
take :: ((m :6: n) ∼ True) => SNat m -> Vec a n -> [a]
take m vec = take (fromSing m) vec

-- The semantics should match that of drop for normal lists.
drop :: ??
drop = undefined

head :: ??
head = undefined

tail :: ??
tail = xs
