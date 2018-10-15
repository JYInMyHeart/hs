import Control.Monad

main = forever $ do
    print "Can u tell me ur name?"
    name <- getLine
    print ("Hello " ++ name)

