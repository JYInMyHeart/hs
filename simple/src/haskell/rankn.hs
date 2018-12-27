{-# LANGUAGE RankNTypes #-}

foo :: (forall a . [a] -> [a]) -> ([b], [c]) -> ([b], [c])
foo f (xs, ys) = (f xs, f ys)


bar :: ([a] -> [a]) -> [a] -> [a]
bar f xs = f xs

bar1 :: (forall a . [a] -> [a]) -> [a] -> [a]
bar1 f xs = f xs

bar2 :: Num b => (forall a . Num a => [a] -> [a]) -> [b] -> [b]
bar2 f xs = f xs

main = print $ bar2 (\xs -> [sum xs]) [1 .. 4]
