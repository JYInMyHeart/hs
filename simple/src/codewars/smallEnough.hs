module Kata where

smallEnough :: [Int] -> Int -> Bool
smallEnough xs v = not (any (> v) xs)
main = print $ smallEnough [1,2,3] 4
