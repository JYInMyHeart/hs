module Min where

import Data.List

minValue :: [Int] -> Int
minValue x =
  read (append (intercalate [] (fmap (take 1) (group $ sort x)))) :: Int

append = concatMap show

main = print $ minValue [1, 2, 3, 1]
