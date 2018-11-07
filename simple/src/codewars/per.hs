import Data.List (delete, nub)

permutations :: String -> [String]
permutations "" = [""]
permutations xs = [x : y | x <- nub xs, y <- permutations $ delete x xs]
