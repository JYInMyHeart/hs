import Control.Applicative
import Data.List as L
import Data.Map.Strict as M
import Data.Set as S

type Pos = (Int, Int)

data Node
  = Passable
  | NotPassable
  deriving (Eq, Show)

type Grid = [[Node]]

type Path = [Pos]

-- shortestPath :: Grid -> Pos -> Pos -> [Path]
-- shortestPath a b c =
--   case dfs a b c [] of
--     Just res -> res
--     Nothing -> []
-- dfs :: Grid -> Pos -> Pos -> [Pos] -> Maybe [Path]
dfs m s e v
  | s == e && m !! y !! x == Passable = Just []
  | x < 0 || y < 0 = Nothing
  | y >= len m = Nothing
  | x >= len (m !! y) = Nothing
  | s `elem` v = Nothing
  | m !! y !! x == NotPassable = Nothing
  | otherwise =
    ((s :) <$> f (x, y - 1)) ++
    ((s :) <$> f (x, y + 1)) ++
    ((s :) <$> f (x - 1, y)) ++ ((s :) <$> f (x + 1, y))
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

main = print $ (dfs (toGrid grid) (0, 0) (4, 4) [])

shortestPath1 [] _ _ = []
shortestPath1 grid start end
  | start == end = [start]
  | otherwise = (`evalState` (S.singleton start)) $ evalContT go
  where
    height = length grid
    width = length $ head grid
    go = callCC $ \k -> bfs [[start]] k
    bfs ::
         [[Pos]]
      -> (Path -> ContT Path (State (S.Set Pos)) b0)
      -> ContT Path (State (S.Set Pos)) Path
    bfs que k = do
      nxt <-
        fmap concat . forM que $ \ps@(q@(x, y):qs) -> do
          dp <- lift get
          fs <-
            flip filterM [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)] $ \pos@(x, y) -> do
              if pos == end
                then do
                  k . reverse $ pos : ps
                  return True -- dummy value
                else do
                  if not (S.member pos dp) &&
                     x >= 0 &&
                     y >= 0 &&
                     x < width && y < height && isPassable (grid !! y !! x)
                    then do
                      lift $ modify (S.insert pos)
                      return True
                    else return False
          return $ (:) <$> fs <*> [ps]
      bfs nxt k
