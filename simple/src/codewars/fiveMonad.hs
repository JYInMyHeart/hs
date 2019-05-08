{-# LANGUAGE NoImplicitPrelude #-}
module Monads where

import Prelude hiding (Monad, Identity, Maybe(..), State, Reader, Writer)
import Data.Monoid

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  
data Identity a = Identity a deriving (Show, Eq)

data Maybe a = Nothing | Just a deriving (Show, Eq)

data State s a = State {runState :: s -> (a, s)}

data Reader s a = Reader {runReader :: s -> a }

data Writer w a = Writer {runWriter :: (a, w)} deriving (Show, Eq)

instance Monad Identity where
  return = Identity
  (Identity v) >>= f = f v
  
instance Monad Maybe where
  return = Just
  Nothing >>= _ = Nothing
  (Just v) >>= f = f v
  
instance Monad (State s) where  
  return a = State (\s -> (a, s))
  (State g) >>= f = State (\s -> let (v, s') = g s in
                                     runState (f v) s')
                                       
instance Monad (Reader s) where
  return a = Reader (\s -> a)
  (Reader g) >>= f = Reader (\s -> runReader (f (g s)) s)
  
instance Monoid w => Monad (Writer w) where
  return a = Writer (a, mempty)
  Writer (v, s) >>= f = let Writer (v', s') = f v in Writer (v', s <> s')
