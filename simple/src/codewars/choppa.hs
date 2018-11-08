{-# LANGUAGE FlexibleContexts #-}

import Control.Monad as CM
import Control.Monad.Loops (whileM_)
import Control.Monad.ST
import Data.Vector as V
import Data.Vector.Mutable as MV

import Data.STRef

import Data.Sequence as Seq

type Pos = (Int, Int)

data Node
  = Passable
  | NotPassable
  deriving (Eq, Show)

type Grid = [[Node]]

type Path = [Pos]

type GridVec = Vector (Vector Node)

gridSize :: GridVec -> (Int, Int)
gridSize x = (V.length x, V.length $ V.head x)

makeVector :: [[Node]] -> GridVec
makeVector rows = V.fromList [V.fromList x | x <- rows]

neighs :: GridVec -> Pos -> [Pos]
neighs grid (x, y) =
  [ (x + dx, y + dy)
  | (dx, dy) <- [(0, 1), (0, -1), (1, 0), (-1, 0)]
  , x + dx >= 0
  , x + dx < n
  , y + dy >= 0
  , y + dy < m
  , (grid V.! (x + dx)) V.! (y + dy) == Passable
  ]
  where
    (n, m) = gridSize grid

getVis :: V.Vector (STVector s [Pos]) -> Pos -> ST s [Pos]
getVis vec (x, y) = do
  let (Just row) = vec V.!? x
  MV.read row y

setVis :: V.Vector (STVector s [Pos]) -> Pos -> [Pos] -> ST s ()
setVis vec (x, y) new_val = do
  let (Just row) = vec V.!? x
  MV.write row y new_val

shortestPathVec :: GridVec -> Pos -> Pos -> Maybe [Pos]
shortestPathVec grid from to =
  runST $ do
    queue <- newSTRef Seq.empty
    let (n, m) = gridSize grid
    visited <- V.replicateM n (MV.replicate m [])
    let (x, y) = from
    modifySTRef queue (|> from)
    setVis visited from [from]
    whileM_ ((not . Seq.null) <$> (readSTRef queue)) $ do
      (current :< rest) <- viewl <$> readSTRef queue
      writeSTRef queue rest
      okNeighs <-
        CM.filterM
          (\x -> (Prelude.null) <$> (getVis visited x))
          (neighs grid current)
      currentPath <- getVis visited current
      CM.forM_ okNeighs (\x -> setVis visited x (x : currentPath))
      CM.forM_ okNeighs (\x -> modifySTRef queue (|> x))
    ans <- getVis visited to
    case ans of
      [] -> pure Nothing
      x -> pure $ Just $ Prelude.reverse x

my_swap :: (a, b) -> (b, a)
my_swap (a, b) = (b, a)

shortestPath :: Grid -> Pos -> Pos -> Path
shortestPath [] _ _ = []
shortestPath ([]:_) _ _ = []
shortestPath grid from to = my_swap <$> ans
  where
    Just ans = (shortestPathVec (makeVector grid) (my_swap from) (my_swap to))
