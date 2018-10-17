module Unflatten where

  {-
  Note haskell doesn't like hetero lists so the return is a list
  of lists of ints.

   [1,4,5,2,1,2,4,5,2,6,2,3,3] -> [[1],[4,5,2,1],[2],[4,5,2,6],[2],[3,3]]

  -}

unflatten :: [Int] -> [[Int]]
unflatten x = case x of
  [] -> []
  (y:ys) ->
     if y < 3 then [y] : unflatten ys else ((y : take (y - 1) ys) : unflatten (drop (y -1) ys))

main = print $ unflatten [1,4,5,2,1,2,4,5,2,6,2,3,3]
