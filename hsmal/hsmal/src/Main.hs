module Main where

import Context
import Evaluator
import Parser
import Syntax
import System.Environment
import Text.Parsec
import TypeChecker

main :: IO ()
main = do
  putStr "user> "
  let ctx = mkContext
  parseTree <- fmap (runParser parseTerm ctx "simpleTyped") getLine
  case parseTree of
    Right expr ->
      case (typeOf ctx expr) of
        Right ex ->
          putStrLn $
          "Evaluating " ++
          (show parseTree) ++ "\n=> " ++ (showTerm ctx . eval) expr
        Left err -> putStrLn $ "Type Error: " ++ (show err)
    Left err -> putStrLn $ "Parsing Error: " ++ (show err)
