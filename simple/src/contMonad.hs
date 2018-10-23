{-# LANGUAGE DeriveFunctor #-}

newtype Cont r a = Cont {runCont :: (a -> r) -> r} deriving Functor

instance Applicative (Cont r) where
  pure a = Cont $ \k -> k a
  cab <*> ca = Cont $ \br -> runCont cab (\ab -> runCont ca (\a -> br (ab a)))

instance Monad (Cont r) where
  return = pure
  ca >>= acb = Cont $ \br -> runCont ca (\a -> runCont (acb a)(\b -> br b))

fact_cps :: Int -> Cont r Int
fact_cps 0 = return 1
fact_cps n = do
  n1 <- fact_cps (n - 1)
  return (n * n1)

plus_1 :: Int -> Cont r Int
plus_1 n = return (n + 1)

div_10 :: Int -> Cont r Int
div_10 n = return (div n 10)

fact_cps1 :: Int -> Cont r Int
fact_cps1 0 = return 1
fact_cps1 n = do
  n1 <- fact_cps1 (n - 1)
  callCC $ \k ->
    let r = n * n1
    in if r > 1000
      then k 0
      else return r

mian = runCont  $ fact_cps1 1001


