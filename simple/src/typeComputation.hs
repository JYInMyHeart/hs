{-# LANGUAGE KindSignatures, GADTs, TypeOperators, DataKinds,
  UndecidableInstances, StandaloneDeriving,
  ExistentialQuantification, TypeFamilies #-}

data Nat = Z | S Nat deriving (Eq,Show)

type family (a :: Nat) + (b :: Nat) :: Nat where
    Z + m = m
    S n + m = n + S m

type family(n :: Nat) * (m :: Nat)::Nat where
    Z * m = Z
    S n * m = (n * m) + m

data Vec a (n :: Nat) where
    Nil :: Vec a Z
    Cons :: a -> Vec a n -> Vec a (S n)

deriving instance Show a => Show (Vec a n)

vhead :: Vec a (S n) -> a
vhead (Cons a v) = a

vtail :: Vec a (S n) -> Vec a n
vtail (Cons x xs) = xs

toList :: Vec a n -> [a]
toList Nil = []
tiList (Cons x xs) = x : toList xs
