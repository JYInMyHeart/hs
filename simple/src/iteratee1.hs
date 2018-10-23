{-# LANGUAGE DeriverFunctor #-}

import Data.Function(fix)
import Control.Monad
import qualified Control.Exception as Exc
import Control.Monad.IO.Class
import Control.Monad.Trans

data Stream a = Chunks [a] | EOF deriving (Show,Eq,Functor)

instance Monoid (Stream a) where
  mempty = Chunks mempty
  mappend (Chunks xs) (Chunks ys) = Chunks (xs ++ ys)
  mappend _ _ = EOF

instance Monad Stream where
  return = Chunks . return
  Chunks xs >>= f = mconcat (fmap f xs)
  EOF >>= _ = EOF