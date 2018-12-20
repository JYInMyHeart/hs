module Syntax where

import           Context
import           Data.Maybe

data Term
  = TermVar Int
            Int
  | TermAbs String
            Type
            Term
  | TermApp Term
            Term
  | TermTrue
  | TermFalse
  | TermIf Term
           Term
           Term
  | TermZero
  | TermSucc Term
  | TermPred Term
  | TermIsZero Term
  | TermUnit
  | TermList [Term]
  | TermAs String Type
  | TermString String
  | TermSet String Term
  | TermPair Term Term
  | TermTuple [Term]
  | TermRecord [Term]
  deriving (Eq, Show)

showTerm :: Context -> Term -> String
showTerm ctx t = case t of
  TermTrue       -> "true"
  TermFalse      -> "false"
  TermZero       -> "0"
  TermUnit       -> "unit"
  TermSucc   t1  -> "(succ " ++ showTerm ctx t1 ++ ")"
  TermPred   t1  -> "(pred " ++ showTerm ctx t1 ++ ")"
  TermList   t1  -> concatMap ((++ "\n") . showTerm ctx) t1
  TermTuple  t1  -> concatMap ((++ "\n") . showTerm ctx) t1
  TermRecord t1  -> concatMap ((++ "\n") . showTerm ctx) t1
  TermPair t1 t2 -> "(" ++ showTerm ctx t1 ++ "," ++ showTerm ctx t2 ++ ")"
  TermAs   t1 t2 -> show t1 ++ " as " ++ show t2
  TermSet  s  t  -> show s ++ " = " ++ showTerm ctx t
  TermIf t1 t2 t3 ->
    "(if "
      ++ showTerm ctx t1
      ++ " then "
      ++ showTerm ctx t2
      ++ " else "
      ++ showTerm ctx t3
      ++ ")"
  TermVar n _ -> fromMaybe "" (getName n ctx)
  TermAbs x tyX t1 ->
    let (x', ctx') = freshVarName x ctx
    in  "(" ++ x' ++ ":" ++ show tyX ++ "." ++ showTerm ctx' t1 ++ ")"
  TermApp t1 t2 -> "(" ++ showTerm ctx t1 ++ " " ++ showTerm ctx t2 ++ ")"
  t             -> show t



type Context = [(String, Binding)]

data Binding
  = NameBinding
  | VarBinding Type
  | ValBinding Term
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
getName n ctx = fmap fst $ getIndex n ctx

getType :: Int -> Context -> Maybe Binding
getType n ctx = fmap snd $ getIndex n ctx

freshVarName :: String -> Context -> (String, Context)
freshVarName x ctx =
  let x' = mkFreshVarName x ctx in (x', addBinding (x', NameBinding) ctx)

mkFreshVarName :: String -> Context -> String
mkFreshVarName x [] = x
mkFreshVarName x ctx@(b : bs) | x == fst b = mkFreshVarName (x ++ "'") ctx
                              | otherwise  = mkFreshVarName x bs
