countChange :: Integer -> [Integer] -> Integer
countChange m s = count s m length s

count :: [Integer] -> Integer -> Integer -> Integer
count 0 _ _ = 1
count [] _ 0 = 0
count s m n = count s m (n - 1) + count s (m - s !! (n - 1)) n
