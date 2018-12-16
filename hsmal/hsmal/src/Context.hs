module Context where

import Control.Monad

data Type
  = TypeBool
  | TypeNat
  | TypeUnit
  | TypeT
  | TypeArrow Type
              Type
  | TypeList [Either TypeError Type]
  deriving (Eq)

instance Show Type where
  show TypeBool = "Bool"
  show TypeNat = "Nat"
  show TypeUnit = "()"
  show TypeT = "T"
  show (TypeList t1) =
    concatMap
      (\x ->
         case x of
           (Right ty) -> show ty ++ "; "
           (Left err) -> show err  ++ "; ")
      t1
  show (TypeArrow tyT1 tyT2) = show tyT1 ++ "->" ++ show tyT2

data TypeError
  = IfGuardNotBool
  | IfArmsTypeMismatch
  | SuccArgNotNat
  | PredArgNotNat
  | IsZeroArgNotNat
  | ArrowParamTypeMismatch
  | TTypeMismatch
  | AppArrowTypeExpected
  | VarTypeErrorWat
  deriving (Eq, Show)

type Context = [(String, Binding)]

data Binding
  = NameBinding
  | VarBinding Type
  deriving (Eq, Show)

mkContext :: Context
mkContext = []

addBinding :: (String, Binding) -> Context -> Context
addBinding = (:)

getBinding :: String -> Context -> Maybe Binding
getBinding = lookup

getIndex :: Int -> Context -> Maybe (String, Binding)
getIndex n ctx | length ctx > n = Just $ ctx !! n
               | otherwise      = Nothing

getName :: Int -> Context -> Maybe String
getName n ctx = liftM fst $ getIndex n ctx

getType :: Int -> Context -> Maybe Binding
getType n ctx = liftM snd $ getIndex n ctx

freshVarName :: String -> Context -> (String, Context)
freshVarName x ctx =
  let x' = mkFreshVarName x ctx in (x', addBinding (x', NameBinding) ctx)

mkFreshVarName :: String -> Context -> String
mkFreshVarName x [] = x
mkFreshVarName x ctx@(b:bs) | x == fst b = mkFreshVarName (x ++ "'") ctx
                            | otherwise  = mkFreshVarName x bs
