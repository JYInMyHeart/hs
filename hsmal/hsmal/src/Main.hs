module Main where

import           Context
import           Evaluator
import           Parser
import           Syntax
import           TypeChecker
import           System.Environment
import           Text.Parsec


main :: IO ()
main = do   let ctx = mkContext
            -- parseTree <- fmap (runParser parseTerm ctx "untyped") (readFile $ "../test/" ++ sourceFile ++ ".txt")
            parseTree <- fmap (runParser parseTerm ctx "untyped") getLine
            case  parseTree of
              Right expr -> 
                case typeOf ctx expr of
                  Right ex -> putStrLn $ "TYPE:" ++ (show ex) ++ "\nAST: " ++ (show parseTree) ++ "\n=> " ++ (showTerm ctx . eval) expr
                  Left err -> putStrLn $  "Type Error: " ++ (show err)
              Left err   -> putStrLn $ "Parsing Error: " ++ (show err)
     
