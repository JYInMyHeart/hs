{-# LANGUAGE LambdaCase #-}
module Context where

import           Control.Monad


data Type
  = TypeBool
  | TypeNat
  | TypeString
  | TypeUnit
  | TypeT
  | TypeArrow Type
              Type
  | TypeList [Either TypeError Type]
  | TypePair (Either TypeError Type) (Either TypeError Type)
  deriving (Eq)

instance Show Type where
  show TypeBool = "Bool"
  show TypeNat = "Nat"
  show TypeUnit = "()"
  show TypeT = "T"
  show TypeString = "String"
  show (TypePair t1 t2) = "(" ++ showEither t1 ++ "," ++ showEither t2 ++ ")"
  show (TypeList t1) =
    concatMap
      showEither
      t1
  show (TypeArrow tyT1 tyT2) = show tyT1 ++ "->" ++ show tyT2

showEither = \case
  (Right ty ) -> show ty ++ ";\n "
  (Left  err) -> show err ++ ";\n "

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

