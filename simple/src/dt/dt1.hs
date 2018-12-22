{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

data Z
data S n

data Nat a where
  Zero :: Nat Z
  Succ :: Nat a -> Nat (S a)

type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (m :+: n)

type family (:*:)(n :: *)(m :: *):: *
type instance Z :*: m  = Z
type instance S n :*: m = m  :+: (n :*: m)

infixl 4 +
(+) :: Nat n -> Nat m -> Nat (n :+: m)
Zero     + a = a
(Succ x) + a = Succ (x + a)

infixl 5 *
(*) :: Nat n -> Nat m -> Nat (n :*: m)
Zero     * _ = Zero
(Succ x) * m = m + x * m

data Vec a n where
    VNil :: Vec a Z
    VCons :: a -> Vec a n -> Vec a (S n)

(++) :: Vec a n -> Vec a m -> Vec a (m :+: n)
VNil       ++ ys = ys
VCons x xs ++ ys = VCons x (xs ++ ys)

repeat :: Nat n -> Vec a m -> Vec a (n :*: m)
repeat Zero     _  = VNil
repeat (Succ x) xs = xs ++ repeat x xs

headV :: Vec a (S n) -> a
headV (VCons x _) = x

tailV :: Vec a (S n) -> Vec a n
tailV (VCons _ xs) = xs

type family (:-:) (n :: *) (m :: *) :: *
type instance n :-: Z = n
type instance Z :-: m = Z
type instance S n :-: S m = n :-: m

type family (:^:) (n :: *) (m :: *) :: *
type instance n :^: Z = n
type instance Z :^: m = Z
type instance S n :^: S m = S (n :^: m)

type family (:~:) (n :: *) (m :: *) :: *
type instance n :~: Z = n
type instance Z :~: m = Z
type instance S n :~: S m = S (n :~: m)


data Equal a b where
    EqZ :: Equal Z Z
    EqS :: Equal a b -> Equal (S a) (S b)


