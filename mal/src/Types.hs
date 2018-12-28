module Types where

import           Data.IORef                     ( IORef )
import qualified Data.Map                      as Map
import           Control.Exception             as CE
import           Control.Monad.Except

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

_get_call ((Func (Fn f) _) : _) = return f
_get_call (MalFunc { fn = (Fn f) } : _) = return f
_get_call _ = throwStr "_get_call first parameter is not a function "

_malfunc ast env params fn = MalFunc
  { fn     = (Fn fn)
  , ast    = ast
  , env    = env
  , params = params
  , macro  = False
  , meta   = Nil
  }
_malfunc_meta ast env params fn meta = MalFunc
  { fn     = (Fn fn)
  , ast    = ast
  , env    = env
  , params = params
  , macro  = False
  , meta   = meta
  }

_fn_Q (MalFunc { macro = False }) = MalTrue
_fn_Q (Func _ _                 ) = MalTrue
_fn_Q _                           = MalFalse

_macro_Q (MalFunc { macro = True }) = MalTrue
_macro_Q _                          = MalFalse


-- Scalars
_nil_Q Nil = MalTrue
_nil_Q _   = MalFalse

_true_Q MalTrue = MalTrue
_true_Q _       = MalFalse

_false_Q MalFalse = MalTrue
_false_Q _        = MalFalse

_symbol_Q (MalSymbol _) = MalTrue
_symbol_Q _             = MalFalse

_string_Q (MalString ('\x029e' : _)) = MalFalse
_string_Q (MalString _             ) = MalTrue
_string_Q _                          = MalFalse

_keyword_Q (MalString ('\x029e' : _)) = MalTrue
_keyword_Q _                          = MalFalse

_number_Q (MalNumber _) = MalTrue
_number_Q _             = MalFalse

-- Lists

_list_Q (MalList _ _) = MalTrue
_list_Q _             = MalFalse

-- Vectors

_vector_Q (MalVector _ _) = MalTrue
_vector_Q _               = MalFalse

-- Hash Maps

_hash_map_Q (MalHashMap _ _) = MalTrue
_hash_map_Q _                = MalFalse

-- Atoms

_atom_Q (MalAtom _ _) = MalTrue
_atom_Q _             = MalFalse
