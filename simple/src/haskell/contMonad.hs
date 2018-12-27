{-# LANGUAGE DeriveFunctor #-}

newtype Cont r a = Cont {runCont :: (a -> r) -> r} deriving Functor

instance Applicative (Cont r) where
  pure a = Cont $ \k -> k a
  cat <*> ca = Cont $ \br -> runCont cab (\ab -> runCont ca (br . ab))

instance Monad (Cont r) where
  return = pure
  ca >>= acb = Cont $ \br -> runCont ca (\a -> runCont (acb a) br)


