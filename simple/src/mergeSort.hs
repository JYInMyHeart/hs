module MergeSort where
import           Control.Monad.Writer

merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

ident :: Int -> ShowS
ident n = showString (replicate (2 * n) ' ')

n1 :: ShowS
n1 = showChar '\n'

mergesort :: Int -> [Int] -> Writer String [Int]
mergesort l [] = return []
mergesort l s@[x] = return [x]
mergesort l s@xs = do
  tell $ (ident l.showString "mergesort: ".shows s.n1) ""
  let (a1,a2) = splitAt (length s `div` 2) xs
  tell $ (ident (l + 1).showString "merge".shows a1.shows a2.n1) ""
  liftM2 merge (mergesort (l+2) a1) (mergesort (l+2) a2)

main = putStrLn $ execWriter (mergesort 0 [5,4,3,6])
