module Syntax where

import           Context

data Term = TermVar Int Int
          | TermAbs  String Type Term
          | TermApp Term Term
          | TermTrue
          | TermFalse
          | TermIf Term Term Term
          | TermZero
          | TermSucc Term
          | TermPred Term
          | TermIsZero Term
          | TermUnit
          deriving (Eq,Show)
        
showTerm :: Context -> Term -> String
showTerm ctx t =
  case t of
    TermTrue         -> "true"
    TermFalse        -> "false"
    TermZero         -> "0"
    TermUnit         -> "unit"
    TermSucc t1      -> "(succ " ++ showTerm ctx t1 ++ ")"
    TermPred t1      -> "(pred " ++ showTerm ctx t1 ++ ")"
    TermIf t1 t2 t3  ->
      "(if " ++ showTerm ctx t1 ++ " then " ++ showTerm ctx t2 ++ " else " ++ showTerm ctx t3 ++ ")"
    TermVar n _      ->
      case getName n ctx of
        Just x  -> x
        Nothing -> ""
    TermAbs x tyX t1 ->
      let (x', ctx') = freshVarName x ctx
      in  "(lambda " ++ x' ++ ":" ++ show tyX ++ "." ++ showTerm ctx' t1 ++ ")"
    TermApp t1 t2    ->
      "(" ++ showTerm ctx t1 ++ " " ++ showTerm ctx t2 ++ ")"
    t -> show t
