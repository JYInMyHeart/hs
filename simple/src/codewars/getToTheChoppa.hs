import Control.Applicative
import Data.List

type Pos = (Int, Int)

data Node
  = Passable
  | NotPassable
  deriving (Eq, Show)

type Grid = [[Node]]

type Path = [Pos]

shortestPath :: Grid -> Pos -> Pos -> Path
shortestPath a b c =
  case dfs a b c [] of
    Just res -> res ++ [c]
    Nothing -> []

dfs :: Grid -> Pos -> Pos -> [Pos] -> Maybe Path
dfs m s e v
  | s == e && m !! y !! x == Passable = Just []
  | x < 0 || y < 0 = Nothing
  | y >= len m = Nothing
  | x >= len (m !! y) = Nothing
  | elem s v = Nothing
  | m !! y !! x == NotPassable = Nothing
  | otherwise =
    (s :) <$> f (x + 1, y) <|> (s :) <$> f (x, y + 1) <|> (s :) <$> f (x - 1, y) <|>
    (s :) <$> f (x, y - 1)
  where
    (x, y) = s
    len = length
    f a = dfs m a e (s : v)

toGrid =
  map
    (map
       (\c ->
          if c == '1'
            then NotPassable
            else Passable)) .
  words

grid = "S0110\n" ++ "01000\n" ++ "01010\n" ++ "00010\n" ++ "0001E"

main = print $ (shortestPath (toGrid grid) (0, 0) (4, 4))
