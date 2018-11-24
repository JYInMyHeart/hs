rowSumOddNumbers :: Integer -> Integer
rowSumOddNumbers n = sum [(s + 1),(s + 3) .. s + (n - 1) * 2 + 1]
  where
    s = sum [2,4 .. (n - 1) * 2]
--rowSumOddNumbers = (^3)
