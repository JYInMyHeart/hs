{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies,FlexibleInstances #-}



class GEq a b where
    equals :: a -> b -> Bool

data Nat = Zero | Succ Nat deriving Show

instance GEq Nat [a] where
    equals a b = eq a b
        where eq Zero [] = True
              eq (Succ n) (_:xs) = eq n xs
              eq _ _ = False

class Fun a b | a -> b where
    fun :: a -> b

instance Fun Int Nat where
    fun a = Zero 


class (Num a,Num b,Num c) => GPlus a b c | a b -> c where
    plus :: a -> b -> c

instance GPlus Int Float Float where
    plus a b = fromIntegral a + b

instance GPlus Float Float Float where
    plus a b = a + b


class Mult a b c | a b -> c where
    mult :: a -> b -> c
data Vector = Vector Int Int Int deriving (Eq,Show)

-- instance Mult Vector Vector Int where
--     mult (Vector x1 y1 z1) (Vector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

instance Mult Vector Vector Vector where
    mult (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 -y1 * x2)

instance Mult Int Vector Vector where
    mult i (Vector x y z) = Vector (i * x) (i * y) (i * z)
m1,m2 :: Vector
m1 = Vector 1 1 1
m2 = Vector 1 1 0

class Collection e ce | ce -> e where
    empty :: ce
    insert :: e -> ce -> ce
    member :: e -> ce -> Bool

instance Eq a => Collection a [a] where
    empty = []
    insert x xs = x : xs
    member = elem




main = print $ (insert 3 empty :: [Int])