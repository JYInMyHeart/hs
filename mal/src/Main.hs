module Main where

import System.IO (hFlush,stdout)
import System.Console.Haskeline
import Control.Monad.Except (runExceptT)
import qualified Data.Map as Map
import Control.Monad.Trans (liftIO,lift)
import Control.Monad (mapM)
import qualified Data.Traversable as DT
import Reader (read_str)
import Printer(_pr_str)
import Types
import Env
--read
mal_read :: String -> IOThrows MalVal
mal_read = read_str

--eval
eval :: MalVal -> Env -> IOThrows MalVal
eval ast env = do
  case ast of
    (MalList   _ _) -> apply_ast ast env
    (MalVector _ _) -> apply_ast ast env
    _               -> eval_ast ast env

let_bind :: Env -> [MalVal] -> IOThrows Env
let_bind env []           = return env
let_bind env (b : e : xs) = do
  evaled <- eval e env
  x      <- liftIO $ env_set env b evaled
  let_bind env xs

eval_ast :: MalVal -> Env -> IOThrows MalVal
eval_ast sym@(MalSymbol _  ) env = env_get env sym
eval_ast ast@(MalList lst m) env = do
  new_lst <- mapM (\x -> eval x env) lst
  return $ MalList new_lst m
eval_ast ast@(MalVector lst m) env = do
  new_lst <- mapM (\x -> eval x env) lst
  return $ MalVector new_lst m
eval_ast ast@(MalHashMap lst m) env = do
  new_hm <- DT.mapM (\x -> (eval x env)) lst
  return $ MalHashMap new_hm m
eval_ast ast env = return ast

apply_ast :: MalVal -> Env -> IOThrows MalVal
apply_ast ast@(MalList [] _) env = do
  return ast
apply_ast ast@(MalList (MalSymbol "def!" : args) _) env = do
  case args of
    (a1@(MalSymbol _) : a2 : []) -> do
      evaled <- eval a2 env
      liftIO $ env_set env a1 evaled
    _ -> throwStr "invalid def!"
apply_ast ast@(MalList (MalSymbol "let*" : args) _) env = do
  case args of
    (a1 : a2 : []) -> do
      params  <- (_to_list a1)
      let_env <- liftIO $ env_new $ Just env
      let_bind let_env params
      eval     a2      let_env
    _ -> throwStr "invalid let*"
apply_ast ast@(MalList _ _) env = do
  el <- eval_ast ast env
  case el of
    (MalList ((Func (Fn f) _) : rest) _) -> f $ rest
    el -> throwStr $ "invalid apply: " ++ show el
apply_ast ast@(MalVector [] _) env = do
  return ast
apply_ast ast@(MalVector _ _) env = do
  el <- eval_ast ast env
  case el of
    (MalVector ((Func (Fn f) _) : rest) _) -> f $ rest
    el -> throwStr $ "invalid apply: " ++ show el

--print
mal_print :: MalVal -> String
mal_print = show

--repl
add [MalNumber a, MalNumber b] = return $ MalNumber $ a + b
add _                          = throwStr "illegal arguements to +"
sub [MalNumber a, MalNumber b] = return $ MalNumber $ a - b
sub _                          = throwStr "illegal arguements to -"
divd [MalNumber a, MalNumber b] = return $ MalNumber $ a `div` b
divd _                          = throwStr "illegal arguements to /"
mult [MalNumber a, MalNumber b] = return $ MalNumber $ a * b
mult _                          = throwStr "illegal arguements to *"






rep :: String -> Env -> IOThrows String
rep line env = do
  ast <- mal_read line
  exp <- eval ast env
  return $ mal_print exp

repl_loop env = do
  line <- getInputLine "user> "
  case line of
    Nothing  -> return ()
    Just ""  -> repl_loop env
    Just str -> do
      let res = runExceptT $ rep str env
      r   <- lift res
      out <- case r of
        Left  (StringError str) -> return $ "Error: " ++ str
        Left  (MalValError mv ) -> return $ "Error: " ++ show mv
        Right val               -> return val
      outputStrLn out
      repl_loop env


main = do
  x <- env_new Nothing
  env_set   x               (MalSymbol "+") $ _func add
  env_set   x               (MalSymbol "-") $ _func sub
  env_set   x               (MalSymbol "*") $ _func mult
  env_set   x               (MalSymbol "/") $ _func divd
  runInputT defaultSettings (repl_loop x)
