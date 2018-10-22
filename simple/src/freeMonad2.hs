{-# LANGUAGE GADTs, KindSignatures #-}

import Control.Monad

data Interaction :: * -> * where
  Say :: String -> (() -> Interaction b) -> Interaction b
  Ask :: (String -> Interaction b) -> Interaction b
  Return :: a -> Interaction a

instance Functor Interaction where
  fmap f (Return a) = Return (f a)
  fmap f (Say str fu) = Say str (\() -> fmap f (fu ()))
  fmap f (Ask fs) = Ask (\str -> fmap f (fs str))

instance Applicatibe Interaction where
  pure = return
  (<*>) = ap

instance Monad Interaction where
  return = Return
  Return x >>= f = f x
  Say msg k >>= f = Say msg ((>>= f) . k)
  Ask k >>= f = Ask ((>>= f) . k)

say :: String -> Interaction ()
say msg = Say msg Return

ask :: Interaction String
ask = Ask Return

test1 = do
  say "who are u"
  a <- ask
  asy $ "hello " ++ a
