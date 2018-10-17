module Kata where

dontGiveMeFive :: Int -> Int -> Int
dontGiveMeFive start end = length [x |  x <- [start..end], not $ containsFive x]

containsFive :: Int -> Bool
containsFive x = '5' `elem` show x
main = print $ dontGiveMeFive 4 17
