module Main where
import           Control.Monad.Writer

left,right :: Int -> (Int,String)
left x = (x - 1,"move left \n")
right x = (x + 1,"move right \n")

move i =
  let (x,str1) = left i
  in let (y,str2) = right x
    in (y,str1 ++ str2)

left',right' :: Int -> Writer String Int
left' x = writer (x - 1,"move left")
right' x = writer (x + 1,"move right")

move' i = do
  x <- left' i
  y <- listen(right' x)
  return y

main = print $ runWriter $ move' 4
