import Control.Monad.Cont

fib_cps :: Int -> ContT r IO Int
fib_cps 0 = return 1
fib_cps 1 = return 1
fib_cps n = do
  n2 <- fib_cps (n - 2)
  liftIO $ putStrLn $ "fib_cps " ++ show (n - 2) ++ "=" ++ show n2
  n1 <- fib_cps (n - 1)
  liftIO $ putStrLn $ "fib_cps " ++ show (n - 1) ++ "=" ++ show n1
  return (n1 + n2)
fact_cps :: Int -> Cont r Int
fact_cps 0 = return 1
fact_cps n = do
  n1 <- fact_cps (n - 1)
  return $ n * n1

fact_cps1 :: Int -> Cont r Int
fact_cps1 0 = return 1
fact_cps1 n = do
  n1 <- fact_cps (n - 1)
  callCC $ \k -> let r = n * n1 in if r > 10000 then k 0 else return r

main = runContT (fib_cps 4) print
