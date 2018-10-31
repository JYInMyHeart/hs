module Codewars.Parentheses where

validParentheses :: String -> Bool
validParentheses (')':xs) = False
validParentheses a = not (any (< 0) (scanl (+) 0 xs)) && sum xs == 0
  where
    xs = v a 0

v :: String -> Int -> [Int]
v a b =
  map
    (\x ->
       if x == '('
         then b + 1
         else if x == ')'
                then b - 1
                else b)
    a

main = print $ validParentheses "(())((()())())"
