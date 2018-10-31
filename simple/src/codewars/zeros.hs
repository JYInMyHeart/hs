module Zeros where

zeros :: Int -> Int
zeros n
  | n >= 5 =
    let n' = n `div` 5
    in n' + zeros n'
  | otherwise = 0

nums :: [Int] -> [Int]
nums = filter (\x -> x `mod` 5 == 0)

le :: [Int] -> Int -> Int
le [] b = b
le a b = le (nums (map (`div` 5) a)) (b + length a)

main = print $ zeros 30

tt n a =
  if n > 5 && n `mod` 5 == 0
    then tt b a + b
    else a + n `mod` 5
  where
    b = n `div` 5
