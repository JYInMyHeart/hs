{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterms #-}

import Control.Monad.ST
import Data.Array (accumArray, bounds, indices)
import qualified Data.Array.ST as MA

type Table = Array Vertex

type Graph = Table [EdgeNode]

type Vertex = Int

type Weight = Int

type Bounds = (Vertex, Vertex)

buildG :: Bounds -> [(Vertex, EdgeNode)] -> Graph
buildG = accumArray (flip (:)) []

mkEmpty ::
     (Ix i, MA.MArray (MA.STUArray s) Bool m)
  => (i, i)
  -> m (MA.STUArray s i Bool)
contains ::
     (Ix i, MA.MArray (MA.STUArray s) Bool m)
  => MA.STUArray s i Bool
  -> i
  -> m Bool
contains = MA.readArray

include :: (Ix i, MA.MArray a Bool m) => a i Bool -> i -> m ()
include arr b = MA.writeArray arr v True
