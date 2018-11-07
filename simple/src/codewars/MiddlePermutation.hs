module MiddlePermutation.JorgeVS.Kata where

import Data.List

middlePermutation :: String -> String
middlePermutation myString = s !! ((length s `div` 2) - 1)
  where
    s = sort $ permutations myString

main = print $ sort $ permutations "abc"
