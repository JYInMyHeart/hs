{-# LANGUAGE TemplateHaskell #-}
moduble Main where
  
sameFactRev :: Int -> [Int]
sameFactRev nmax -- your code

primeFactors :: Int ->  [Int]
primeFactors x = [x | x `mod` a == 0,a <- [2,3,5,7,11,13,17,19,23,29,31,37]]

main = print $ primeFactors 1089