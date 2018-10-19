append :: [a] -> [a] -> [a]
append [] x     = x
append x  []    = x
append x (y:ys) = append (x ++ [y])  ys

main = print $ append [1,2,3] [4,5,6]
