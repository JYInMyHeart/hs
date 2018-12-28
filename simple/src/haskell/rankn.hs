{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.ST
import Control.Monad.State
import Data.STRef

foo :: (forall a . [a] -> [a]) -> ([b], [c]) -> ([b], [c])
foo f (xs, ys) = (f xs, f ys)


bar :: ([a] -> [a]) -> [a] -> [a]
bar f = f

bar1 :: (forall a . [a] -> [a]) -> [a] -> [a]
bar1 f = f

bar2 :: Num b => (forall a . Num a => [a] -> [a]) -> [b] -> [b]
bar2 f = f

factorial :: Int -> STRef s Int -> ST s Int
factorial n accRef = do
  numRef <- newSTRef n
  num    <- readSTRef numRef
  if num < 1
    then readSTRef accRef
    else do
      acc <- readSTRef accRef
      writeSTRef accRef    (acc * n)
      writeSTRef numRef    (num - 1)
      factorial  (num - 1) accRef

fact :: Int -> Int
fact n = runST $ do
  accRef <- newSTRef 1
  factorial n accRef

factorial1 :: Int -> State Int Int
factorial1 n = do
  num <- get
  if n <= 1
    then return 1
    else do
      put $ n - 1
      fmap (* n) (factorial1 $ n - 1)

factorial11 :: State Int Int
factorial11 = get
  >>= \x -> if x <= 1 then return 1 else put (x - 1) >> fmap (* x) factorial11
fact1 :: Int -> Int
fact1 n = evalState (factorial1 n) n

fact2 :: Int -> Int
fact2 = evalState factorial11

factorial111 :: Int -> State (Int, Int) Int
factorial111 x = do
  (s, num) <- get
  if x <= 1
    then return 1
    else do
      put (s * num, num - 1)
      factorial111 $ x - 1

type GameValue = Int
type GameState = (Bool, Int)

playGame :: String -> State GameState GameValue
playGame [] = do
  (_, score) <- get
  return score

playGame (x : xs) = do
  (on, score) <- get
  case x of
    'a' | on -> put (on, score + 1)
    'b' | on -> put (on, score - 1)
    'c'      -> put (not on, score)
    _        -> put (on, score)
  playGame xs


startState = (False, 0)
-- main = print $ runState (factorial111 3) (1, 3)

-- let applyToTuple f (a@(x:xs) ,b@(y:ys)) = (f a,f b) :: (Int,Int)

applyToTuple :: forall a b c . (forall a . [a] -> Int) -> ([b], [c]) -> (Int, Int)
applyToTuple f (x, y) = (f x, f y)


-- main = print $ applyToTuple length ([1, 2], "abc")

foob :: forall a b . (b -> b) -> b -> (a -> b) -> Maybe a -> b
foob postProcess onNothin onJust mval = postProcess val
 where
  val :: b
  val = maybe onNothin onJust mval

main = print $ applyToTuple length ([1, 2], "abc")
