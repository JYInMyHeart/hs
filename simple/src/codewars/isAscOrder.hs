module NumbersInOrder (isAscOrder) where
import           Data.List

isAscOrder :: [Int] -> Bool
isAscOrder x = x == sort x

main = print $ isAscOrder [5,2,3,4]
