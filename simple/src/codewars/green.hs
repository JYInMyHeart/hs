module Green where

gen5 n m =
  (if d /= 0
     then (n' :)
     else id) $
  gen5 n' m'
  where
    d = (n * n `div` m) `mod` 10
    m' = m * 10
    n' = d * m + n

gen6 n m =
  (if d /= 0
     then (n' :)
     else id) $
  gen6 n' m'
  where
    d = negate (n * n `div` m) `mod` 10
    m' = m * 10
    n' = d * m + n

merge (a:as) (b:bs) =
  if a <= b
    then a : merge as (b : bs)
    else b : merge (a : as) bs

greens = (1 : 5 : 6 : merge (gen5 5 10) (gen6 6 10))

green :: Int -> Integer
green n = greens !! (n - 1)

main = print $ green 5
