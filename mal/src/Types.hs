module Types where

import Data.IORef (IORef)
import qualified Data.Map as Map
import Control.Exception as CE
import Control.Monad.Except

--Base Mal types

newtype Fn = Fn ([MalVal] -> IOThrows MalVal)
data MalVal = Nil
            | MalFalse
            | MalTrue
            | MalNumber Int
            | MalString String
            | MalSymbol String
            | MalList [MalVal] MalVal
            | MalVector [MalVal] MalVal
            | MalHashMap (Map.Map String MalVal) MalVal
            | MalAtom (IORef MalVal) MalVal
            | Func Fn MalVal
            | MalFunc {fn :: Fn,
                      ast :: MalVal,
                      env :: Env,
                      params ::MalVal,
                      macro :: Bool,
                      meta::MalVal}


_equal_Q Nil              Nil              = True
_equal_Q MalFalse         MalFalse         = True
_equal_Q MalTrue          MalTrue          = True
_equal_Q (MalNumber a   ) (MalNumber b   ) = a == b
_equal_Q (MalString a   ) (MalString b   ) = a == b
_equal_Q (MalSymbol a   ) (MalSymbol b   ) = a == b
_equal_Q (MalList    a _) (MalList    b _) = a == b
_equal_Q (MalList    a _) (MalVector  b _) = a == b
_equal_Q (MalVector  a _) (MalVector  b _) = a == b
_equal_Q (MalVector  a _) (MalList    b _) = a == b
_equal_Q (MalHashMap a _) (MalHashMap b _) = a == b
_equal_Q (MalAtom    a _) (MalAtom    b _) = a == b
_equal_Q _                _                = False


instance Eq MalVal where
  x == y = _equal_Q x y

--Errors/Exceptions

data MalError = StringError String
              | MalValError MalVal

type IOThrows = ExceptT MalError IO

throwStr :: String -> IOThrows a
throwStr str = throwError $ StringError str
throwMalVal :: MalVal -> IOThrows a
throwMalVal mv = throwError $ MalValError mv

--Env
data EnvData = EnvPair(Maybe Env,(Map.Map String MalVal))
type Env = IORef EnvData


_func fn = Func (Fn fn) Nil
_func_meta fn meta = Func (Fn fn) meta


_to_list (MalList   lst _) = return lst
_to_list (MalVector lst _) = return lst
_to_list _                 = throwStr "_to_list expected a MalList or MalVector"
