-- {-# LANGUAGE FlexibleInstances,UndecidableInstances #-}
-- import Data.Default

-- instance Monoid a => Default a where
--     def = mempty

-- newtype S = S Int deriving (Show, Eq)

-- instance Monoid S where
--     mempty = S 0
--     mappend (S n) (S m) = S (n + m)

-- instance Default a => Monoid a

data Tree a = Leaf a | Node (Tree a) a (Tree a)
    deriving (Show)

instance Foldable Tree where
    foldMap f (Leaf x) = f x
    foldMap f (Node l n r) = foldMap f l `mappend` f n `mappend` foldMap f r
tree = Node (Leaf 1) 2 (Leaf 3)

flatten a = foldMap (: []) a

traverse1 _ [] = pure []
traverse1 f (x:xs) = (:) <$> f x <*> traverse1 f xs

sequenceA1 [] = pure []
sequenceA1 (x:xs) = (:) <$> x <*> sequenceA1 xs



main = print $ traverse1 (\x -> if x == 0 then Nothing else Just (x + 1)) [1,2,3,4,5]