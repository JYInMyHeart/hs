import Data.List

nextSmaller :: Integer -> Maybe Integer
nextSmaller n
  | not (null set) = Just (last set)
  | otherwise = Nothing
  where
    set = sets n

sets n =
  map
    (\x -> read x :: Integer)
    (filter
       (\x -> (read x :: Integer) < n)
       (filter (\x -> take 1 x /= "0") (sort $ permutations (show n))))

nextSmaller1 :: Integer -> Maybe Integer
nextSmaller1 n = go (n - 1)
  where
    go c
      | c < m || (length $ show c) < (length $ show n) = Nothing
      | otherwise =
        if helper c == m
          then Just c
          else go (c - 1)
      where
        m = helper n
        helper n = read (take 1 zs ++ ys ++ drop 1 zs) :: Integer
          where
            zs = sort $ filter (`elem` "123456789") xs
            xs = show n
            ys = filter (== '0') xs

main = print $ nextSmaller 1234567908
