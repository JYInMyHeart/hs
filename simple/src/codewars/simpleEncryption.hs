encrypt :: String -> Int -> String
encrypt x 0 = x
encrypt x n = encrypt (en x) (n - 1)

en x = (map snd (values1 x)) ++ (map snd (values x))

maps x = zip [0 .. (length x)] x

values x = filter (\(a, b) -> a `mod` 2 == 0) (maps x)

values1 x = filter (\(a, b) -> a `mod` 2 /= 0) (maps x)

decrypt :: String -> Int -> String
decrypt x 0 = x
decrypt x n = decrypt (de x) (n - 1)

de x = (map snd (values x)) ++ (map snd (values1 x))

main = print $ decrypt "hsi  etTi sats" 1
-- decrypt :: String -> Int -> String
-- decrypt -- write your code here --
