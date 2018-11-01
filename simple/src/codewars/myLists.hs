module Folded.MyLists where
foldList :: [Int] -> Int -> [Int]
foldList xs 0 = xs
foldList xs n = foldList (foldLN xs) (n - 1)

f :: [Int] -> [Int]
f xs = zipWith (curry sum') (reverse $ snd two) (fst two)
  where two = di xs

di xs = d $ splitAt (length xs `div` 2) xs

d (a, b)
  | length a > length b = (a,0 : b)
  | length a < length b = (a ++ [0],b)
  | otherwise = (a,b)
sum' (a,b) = a + b

foldLN :: [Int] -> [Int]
foldLN []     = []
foldLN [x]    = [x]
foldLN (x:xs) = (x + last xs) : foldLN (init xs)

main = print $ (!!)  (iterate foldLN [1,2,3,4,5]) 2
