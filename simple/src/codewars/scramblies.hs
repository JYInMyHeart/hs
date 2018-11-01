module Codewars.G964.Scramblies where
import           Data.List
scramble :: String -> String -> Bool
scramble s1 s2 =  null (s2 \\ s1)
-- scramble s1 s2 =  not (any (\ x -> snd x < 0) (mu (s s1) (s s2)))

s :: String -> [(Char,Int)]
s a = map (\x -> (head x,length x)) (group $ sort a)

mu :: [(Char,Int)] -> [(Char,Int)] -> [(Char,Int)]
mu a = concatMap (uncurry (put a))


get :: [(Char,Int)] -> Char -> Int
get [] _ = 0
get (y:ys) x
  | x == fst y = snd y
  | otherwise = get ys x

put :: [(Char,Int)] -> Char -> Int -> [(Char,Int)]
put [] k v = [(k,-v)]
put (x:xs) k v
  | fst x == k = (k,snd x - v) : xs
  | otherwise = put xs k v



-- main = print $    put (s "katas")  'a' 2
-- main = print $  mu  (s "katas") (s "steak")
main = print $  scramble  "katas" "steak"
