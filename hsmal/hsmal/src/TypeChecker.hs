module TypeChecker where

import           Context
import           Evaluator
import           Syntax

typeOf :: Context -> Term -> Either TypeError Type
typeOf _ TermTrue  = Right TypeBool
typeOf _ TermFalse = Right TypeBool
typeOf _ TermUnit  = Right TypeUnit
typeOf ctx (TermIf t1 t2 t3) =
  if eitherTypeEquals (typeOf ctx t1) (Right TypeBool)
    then if typeOf ctx t2 == typeOf ctx t3
      then typeOf ctx t2
      else Left IfArmsTypeMismatch
    else Left IfGuardNotBool
typeOf ctx (TermVar x _) = case getType x ctx of
  Just (VarBinding tyT) -> Right tyT
  _                     -> Left VarTypeErrorWat
typeOf ctx (TermAbs x tyT1 t2) =
  let ctx' = addBinding (x, VarBinding tyT1) ctx
      tyT2 = typeOf ctx' t2
  in  case tyT2 of
        Right tyT2'   -> Right $ TypeArrow tyT1 tyT2'
        Left  tyErrT2 -> Left tyErrT2
typeOf ctx (TermApp t1 t2) =
  let tyT1 = typeOf ctx t1
      tyT2 = typeOf ctx t2
  in  case tyT1 of
        Right (TypeArrow tyT11 tyT12) -> case tyT2 of
          (Right tyT2') ->
            if tEquals tyT2' tyT11 then Right tyT12 else Left TTypeMismatch
          _ -> Left ArrowParamTypeMismatch
        Right ttt -> Right TypeT
        _         -> Left AppArrowTypeExpected
typeOf ctx TermZero = Right TypeNat
typeOf ctx (TermSucc t1)
  | eitherTypeEquals (typeOf ctx t1) (Right TypeNat) = Right TypeNat
  | otherwise = Left SuccArgNotNat
typeOf ctx (TermPred t1)
  | eitherTypeEquals (typeOf ctx t1) (Right TypeNat) = Right TypeNat
  | otherwise = Left PredArgNotNat
typeOf ctx (TermIsZero t1)
  | eitherTypeEquals (typeOf ctx t1) (Right TypeNat) = Right TypeBool
  | otherwise = Left IsZeroArgNotNat
typeOf _   (TermString t  ) = Right TypeString
typeOf ctx (TermList   t1 ) = Right $ TypeList (fmap (typeOf ctx) t1)
typeOf ctx (TermTuple  t1 ) = Right $ TypeList (fmap (typeOf ctx) t1)
typeOf ctx (TermRecord t1 ) = Right $ TypeList (fmap (typeOf ctx) t1)
typeOf ctx (TermAs   s  t ) = Right t
typeOf ctx (TermSet  s  t ) = typeOf ctx t
typeOf ctx (TermPair t1 t2) = Right $ TypePair (typeOf ctx t1) (typeOf ctx t2)



typeEquals :: Type -> Type -> Bool
typeEquals t1 t2 = case t2 of
  TypeT          -> t1 == TypeT
  TypeBool       -> t1 == TypeT || t1 == TypeBool
  TypeNat        -> t1 == TypeT || t1 == TypeNat
  TypeUnit       -> t1 == TypeT || t1 == TypeUnit
  TypeString     -> t1 == TypeT || t1 == TypeString
  (TypeList tt1) -> case t1 of
    (TypeList tt2) -> listTypesEquals tt1 tt2 (length tt1)
    _              -> False
  (TypePair t11 t12) -> case t1 of
    (TypePair t21 t22) ->
      (eitherTypeEquals t11 t21 && eitherTypeEquals t12 t22)
        || (eitherTypeEquals t21 t11 && eitherTypeEquals t22 t12)
    _ -> False
  (TypeArrow t11 t12) -> case t1 of
    (TypeArrow t21 t22) ->
      (typeEquals t11 t21 && typeEquals t12 t22)
        || (typeEquals t21 t11 && typeEquals t22 t12)
    _ -> False

tEquals t1 t2 = typeEquals t1 t2 || typeEquals t2 t1

eitherTypeEquals :: Either TypeError Type -> Either TypeError Type -> Bool
eitherTypeEquals t1 t2 = case t1 of
  Right t11 -> case t2 of
    Right t22 -> tEquals t11 t22
    Left  _   -> False
  Left _ -> False

listTypesEquals
  :: [Either TypeError Type] -> [Either TypeError Type] -> Int -> Bool
listTypesEquals t1 t2 n
  | n == 0 = True
  | n > 0 =  eitherTypeEquals (t1 !! (n - 1)) (t2 !! (n - 1))
  && listTypesEquals t1 t2 (n - 1)
  | otherwise = False

