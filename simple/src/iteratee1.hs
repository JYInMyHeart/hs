{-# LANGUAGE DeriverFunctor #-}

import qualified Control.Exception      as Exc
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Data.Function          (fix)

data Stream a = Chunks [a] | EOF deriving (Show,Eq,Functor)

instance Monoid (Stream a) where
  mempty = Chunks mempty
  mappend (Chunks xs) (Chunks ys) = Chunks (xs ++ ys)
  mappend _ _                     = EOF

instance Monad Stream where
  return = Chunks . return
  Chunks xs >>= f = mconcat (fmap f xs)
  EOF >>= _ = EOF

--  liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- liftA2 (<$>) :: (a1 -> b1) -> (f1 a1 -> f1 b1) -> f (a1 -> b1) -> f f1 a1 -> f f1 b1
