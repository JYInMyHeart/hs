module Main where
import Data.Char
import System.Environment (getArgs)
import Cal

main :: IO ()
main = do
    expr <- getArgs 
    print $ calculate $ concat expr
