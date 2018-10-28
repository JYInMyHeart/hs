import Data.Char

solution :: String -> Int
solution "" = 0
solution str = num + solution xs
  where
    (num, xs):_ =
      [(num, drop (length n) str) | (n, num) <- table, n `isPrefixOf` str]
    table =
      zip
        (words "M CM D CD C XC L XL X IX V IV I")
        [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]
