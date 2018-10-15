
{-# LANGUAGE FlexibleInstances #-}

class Printf t where
    printf :: String -> t 

-- instance Printf (IO ()) where
--     printf1 a = putStrLn a

-- instance Show t => Printf (t -> IO ()) where
--     printf1 cs x = putStrLn (format cs x)

instance {-# OVERLAPPING #-} (Show u,Show t) => Printf (u -> t ) where
    printf cs  = \x -> printf (format cs x)

format :: Show t => String -> t -> String
format ('%':'s':cs) cs' = show cs' ++ cs
format (c:cs) cs' = c : format cs cs'
format "" cs' = ""
test2 :: IO ()
test2 = printf  "we formatted an integer: %s " "1" 
main =  test2