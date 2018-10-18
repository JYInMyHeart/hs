module Main where
import           Cal                (calculate)
import           Data.Char
import           System.Environment (getArgs)

main :: IO ()
main = do
    expr <- getArgs
    print $ calculate $ concat expr


