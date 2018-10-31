module Codwars.Kata.Duplicates where

import Data.Char
import Data.List

duplicateCount :: String -> Int
duplicateCount = ans

ans a = length $ filter (> 1) (map length (group $ sort (map toUpper a)))

main = print $ duplicateCount "abc"
