import           Control.Monad.Cont

print4 :: ContT r IO()
print4 = do
  (goto,n) <-
    callCC $ \k ->
      let f x = k (f, x)
      in return (f, 0)
  if(n < 4)
    then do
      lift $ putStrLn "hello"
      goto (n + 1)
    else return ()

fact_cps2 :: Int -> Cont r Int
fact_cps2 n = do
  (goto,acc,num) <-
    callCC $ \k ->
      let f x y = k (f ,x,y)
      in return (f,1,n)
  if num == 1
    then return acc
    else goto (acc * num) (num - 1)

main = print $ runCont (fact_cps2 5) id
