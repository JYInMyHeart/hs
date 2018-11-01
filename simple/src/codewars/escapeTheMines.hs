module EscapeTheMines where

import Control.Applicative
import Data.List

type XY = (Int, Int)

data Move
  = U
  | D
  | R
  | L
  deriving (Eq, Show)

solve :: [[Bool]] -> XY -> XY -> [Move]
solve grid s e = v (dfs grid [] s e)

v :: Maybe a -> a
v (Just b) = b
v Nothing = error " e"

dfs :: [[Bool]] -> [XY] -> XY -> XY -> Maybe [Move]
dfs m v s e
  | s == e = Just []
  | s `elem` v = Nothing
  | fst s >= length m || snd s >= length ((!!) m 0) || fst s < 0 || snd s < 0 =
    Nothing
  | not (value s m) = Nothing
  | otherwise =
    ((R :) <$> dfs m (s : v) (x + 1, y) e) <|>
    ((D :) <$> dfs m (s : v) (x, y + 1) e) <|>
    ((L :) <$> dfs m (s : v) (x - 1, y) e) <|>
    ((U :) <$> dfs m (s : v) (x, y - 1) e)
  where
    x = fst s
    y = snd s

value :: XY -> [[Bool]] -> Bool
value (a, b) g = (!!) ((!!) g a) b
