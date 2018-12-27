module Core where

import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           Control.Exception              ( catch )
import           Control.Monad.Trans            ( liftIO )
import qualified Data.Map                      as Map
import           Data.Time.Clock.POSIX          ( getPOSIXTime )
import           Data.IORef                     ( IORef
                                                , newIORef
                                                , readIORef
                                                , writeIORef
                                                )

import           Reader                         ( read_str )
import           Types
import           Printer                        ( _pr_str
                                                , _pr_list
                                                )

equal_Q [a, b] = return $ if a == b then MalTrue else MalFalse
equal_Q _      = throwStr "illegal arguement to ="

run_1 :: (MalVal -> MalVal) -> [MalVal] -> IOThrows MalVal
run_1 f (x : []) = return $ f x
run1 _ _ = throwStr "function takes a single arguement"

run_2 :: (MalVal -> MalVal -> MalVal) -> [MalVal] -> IOThrows MalVal
run_2 f (x : y : []) = return $ f x y
run_2 _ _            = throwStr "function takes a two arguements"


throw (mv : []) = throwMalVal mv
throw _         = throwStr "illegal arguements to throw"

symbol (MalString str : []) = return $ MalSymbol str
symbol _                    = throwStr "symbol called with non-string"

ketword (MalString ('\x029e' : str) : []) =
  return $ MalString $ "\x029e" ++ str
keyword (MalString str : []) = return $ MalString $ "\x029e" ++ str
keyword _                    = throwStr "keyword called with non-string"

pr_str args = do
  return $ MalString $ _pr_list True " " args

str args = do
  return $ MalString $ _pr_list False "" args

prn args = do
  liftIO $ putStrLn $ _pr_list True " " args
  liftIO $ hFlush stdout
  return Nil

println args = do
  liftIO $ putStrLn $ _pr_list False " " args
  liftIO $ hFlush stdout
  return Nil



num_op op [MalNumber a, MalNumber b] = do
  return $ MalNumber $ op a b
num op _ _ = throwStr "illegal arguements to number operation"

cmp_op op [MalNumber a, MalNumber b] = do
  return $ if op a b then MalTrue else MalFalse
cmp_op _ _ = throwStr "illegal arguements to comparison operation"


time_ms _ = do
  t <- liftIO $ getPOSIXTime
  return $ MalNumber $ round (t * 1000)

ns =
  [ ("="          , _func equal_Q)
  , ("throw"      , _func throw)
  , ("nil?"       , _func $ run_1 $ _nil_Q)
  , ("true?"      , _func $ run_1 $ _true_Q)
  , ("false?"     , _func $ run_1 $ _false_Q)
  , ("string?"    , _func $ run_1 $ _string_Q)
  , ("symbol"     , _func $ symbol)
  , ("symbol?"    , _func $ run_1 $ _symbol_Q)
  , ("keyword"    , _func $ keyword)
  , ("keyword?"   , _func $ run_1 $ _keyword_Q)
  , ("number?"    , _func $ run_1 $ _number_Q)
  , ("fn?"        , _func $ run_1 $ _fn_Q)
  , ("macro?"     , _func $ run_1 $ _macro_Q)
  , ("pr-str"     , _func pr_str)
  , ("str"        , _func str)
  , ("prn"        , _func prn)
  , ("println"    , _func println)
  ,
    -- ("readline", _func do_readline),
    ("read-string", _func (\[(MalString s)] -> read_str s))
  ,
    -- ("slurp", _func slurp),
    ("<"          , _func $ cmp_op (<))
  , ("<="         , _func $ cmp_op (<=))
  , (">"          , _func $ cmp_op (>))
  , (">="         , _func $ cmp_op (>=))
  , ("+"          , _func $ num_op (+))
  , ("-"          , _func $ num_op (-))
  , ("*"          , _func $ num_op (*))
  , ("/"          , _func $ num_op div)
  , ("time-ms"    , _func time_ms)
  ]
