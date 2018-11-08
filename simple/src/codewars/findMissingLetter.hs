module Kata where

import Data.List

findMissingLetter :: [Char] -> Char
findMissingLetter cs = val cs \\ cs

val cs = [(head cs) .. (last cs)]

main = print $ val "abcef"
