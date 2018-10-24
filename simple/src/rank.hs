{-# LANGUAGE Rank2Types #-}

import Control.Monad.ST
import Data.STRef

factorial :: Int -> STRef s Int -> ST s Int
factorial n accRef = do
  numRef <- newSTRef n
  num <- readSTRef numRef
  if num < 1
    then readSTRef accRef
    else do
      acc <- readSTRef accRef
      writeSTRef accRef (acc * n)
      writeSTRef numRef (num - 1)
      factorial (num - 1) accRef

fact :: Int -> Int
fact n =
  runST $ do
    accRef <- newSTRef 1
    factorial n accRef

foo :: (forall a. [a] -> [a]) -> ([b], [c]) -> ([b], [c])
foo f (xs, ys) = (f xs, f ys)

main = print $ foo (take 2) ([1, 2, 3], "456")
