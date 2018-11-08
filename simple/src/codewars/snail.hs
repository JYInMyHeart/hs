import Data.List

snail :: [[Int]] -> [Int]
snail [] = []
snail (xs:xss) = xs ++ (snail . reverse . transpose) xss
