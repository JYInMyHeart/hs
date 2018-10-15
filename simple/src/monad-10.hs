{-# LANGUAGE DeriveFunctor #-}
import           Data.List (group, sort)
newtype Identity a = Identity {runIdentity :: a} deriving (Functor,Show)

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f)(Identity a) = Identity (f a)

instance Monad Identity where
    return = Identity
    Identity m >>= k = k m

mostFrequentItemCount :: [Int] -> Int
mostFrequentItemCount [] = 0
mostFrequentItemCount xs = max1 [times1 y xs | y <- xs]

max1 :: (Eq a,Ord a,Num a) => [a] -> a
max1 [] = 0
max1 (x:xs) | x > y = x
           | otherwise = y
           where y = max1 xs


times1 :: (Eq a,Num a )=> a -> [a] -> a
times1 _ []     = 0
times1 b (x:xs) = if b == x then times1 b xs + 1  else times1 b xs


divisors :: (Show a, Integral a) => a -> Either String [a]
divisors a = case filter ((== 0) . rem a) [2..a`div`2] of
    [] -> Left $ show a ++ " is prime"
    xs -> Right xs
-- divisors a  | null ans = Left  (show a ++ " is prime")
--             | otherwise = Right ans
--         where ans = filter (\x -> mod a x == 0) [2..a `div` 2]


delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (l:ls) | x == l = ls
                | otherwise = l:delete x ls

selectionSort [] = []
selectionSort xs = mini : selectionSort xs'
    where mini = minimum xs
          xs'  = delete mini xs

-- main = print $ Identity 5 >>= \a -> Identity (odd a) >>= \b -> Identity (not b)
main = print $ selectionSort [1,4,6,43,2]
