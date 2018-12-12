module Main where

import           Context
import           Evaluator
import           Parser
import           Syntax
import           TypeChecker
import           System.Environment
import           Text.Parsec

main :: IO ()
main =
  do putStr "user> "
     args <- getLine
     case args of
       sourceFile ->
         do let ctx = mkContext
            parseTree <- fmap (runParser parseTerm ctx "untyped") (readFile $ "../test/" ++ sourceFile ++ ".txt")
            case  parseTree of
              Right expr -> 
                case (typeOf ctx expr) of
                  Right ex -> putStrLn $ "Evaluating " ++ (show parseTree) ++ "\n=> " ++ (showTerm ctx . eval) expr
                  Left err -> putStrLn $ "Type Error: " ++ (show err)
              Left err   -> putStrLn $ "Parsing Error: " ++ (show err)
     
