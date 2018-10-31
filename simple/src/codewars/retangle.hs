module Codewars.Kata.Rectangle where

squaresInRect :: Integer -> Integer -> Maybe [Integer]
squaresInRect lng wdth =
  if lng == wdth
    then Nothing
    else Just (ans lng wdth [])

ans :: Integer -> Integer -> [Integer] -> [Integer]
ans 0 _ c = c
ans _ 0 c = c
ans a b c =
  if a > b
    then b : ans (a - b) b c
    else a : ans a (b - a) c

main = print $ ans 20 14 []
