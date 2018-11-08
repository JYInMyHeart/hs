import Data.List

smallest :: Integer -> (Integer, Int, Int)
smallest n = minimum [(read . f i j . show $ n, i, j) | i <- is, j <- is]
  where
    is = [0 .. pred . length . show $ n]
    f i j xs =
      let (e, l) = g i xs
      in h j e l
    g i xs = (xs !! i, take i xs ++ drop (i + 1) xs)
    h i e xs = take i xs ++ [e] ++ drop i xs

-- smallest n
--   | f1 tt1 < h n = tt1
--   | otherwise = (h n, 0, length (show n))
--   where
--     tt1 = tv1 n
t1 n = minimum xs : r xs
  where
    xs = show n

tv1 n = (read (t1 n) :: Integer, v (min1 minimum (show n)), 0)

f1 (a, b, c) = a

h n = read (drop 1 (show n) ++ [head (show n)]) :: Integer

min1 :: (String -> Char) -> String -> Maybe Int
min1 f xs = elemIndex (f xs) xs

v :: Maybe Int -> Int
v Nothing = -1
v (Just a) = a

r :: String -> String
r xs = delete (minimum xs) xs

main = print $ smallest 213145
