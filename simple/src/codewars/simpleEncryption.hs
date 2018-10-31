import Data.List

encrypt :: String -> Int -> String
encrypt x 0 = x
encrypt x n = encrypt (en x) (n - 1)

en x = map snd a ++ map snd b
  where
    a = snd t
    b = fst t
    t = values x

maps x = zip [0 .. (length x)] x

values x =
  ( (filter (\(a, b) -> a `mod` 2 == 0) t)
  , (filter (\(a, b) -> a `mod` 2 /= 0) t))
  where
    t = maps x

decrypt :: String -> Int -> String
decrypt x 0 = x
decrypt x n = decrypt (de x) (n - 1)

de1 x = uncurry zip a
  where
    a = di x

di x = splitAt (length x `div` 2) x

swap (a, b) = (b, a)

combine (a, b) = [a, b]

de2 x = map (combine . swap) (de1 x)

de x = concat (de2 x)

main = print $ encrypt "This is a Test" 1
