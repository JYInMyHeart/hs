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
  sourceFile <- getLine
  let ctx = mkContext
  parseTree <- fmap (runParser parseList ctx "simpletyped")
                    (readFile $ "../test/" ++ sourceFile ++ ".txt")
            -- parseTree <- fmap (runParser parseTerm ctx "untyped") getLine
  case parseTree of
    Right expr -> case typeOf ctx expr of
      Right ex ->
        putStrLn
          $ "|---------------------------------TYPE------------------------------------| \n"
          ++ show ex
          ++ "\n|----------------------------------AST-----------------------------------|\n"
          ++ show parseTree
          ++ "\n"
          ++ "\n|----------------------------------VALUE----------------------------------|\n"
          ++ (showTerm ctx . eval) expr
      Left err -> putStrLn $ "Type Error: " ++ show err
    Left err -> putStrLn $ "Parsing Error: " ++ show err
