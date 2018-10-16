
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}

class Printf t where
    printf :: String -> t

instance Printf (IO ()) where
    printf = putStrLn

instance Show t => Printf (t -> IO ()) where
    printf cs x = putStrLn (format cs x)
-- {-# OVERLAPPING #-}
instance (Show u,Printf t) => Printf (u ->  t) where
    printf cs  = printf . format cs

format :: Show t => String -> t -> String
format ('%':'s':cs) cs' = show cs' ++ cs
format (c:cs) cs'       = c : format cs cs'
format "" cs'           = ""
test2 :: IO ()
test2 =  printf  "we formatted an integer: %s %s %s %s" "1" "2" "3" (1,2)
main =   test2
