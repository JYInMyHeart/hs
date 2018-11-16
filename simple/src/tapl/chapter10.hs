
data Ty = TyBool | TyArr Ty Ty deriving Show

data Term =
  TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmVar Int Int
  | TmAbs String Ty Term
  | TmApp Term Term
  deriving Show

type Context = [(String,Ty)]


typeOf ctx t = case t of
  TmTrue -> TyBool
  TmFalse -> TyBool
  TmIf t1 t2 t3 -> if (typeOf ctx t1) == TyBool
    then
      let tyT2 = typeOf ctx t2 in
        if tyT2 == (typeOf ctx t3)
          then tyT2
          else error "arms of conditional have differernt types"
    else error "guard of conditional not a boolean"
  TmVar i _ -> getTypeFromContext ctx i
  TmAbs x tyT1 t2 ->
    let ctx' = addbinding ctx x  tyT1 in
      let tyT2 = typeOf ctx' t2 in
        TyArr tyT1 tyT2
  TmApp t1 t2 ->
    let tyT1 = typeOf ctx t1 in
      let tyT2 = typeOf ctx t2 in
        case tyT1 of
          TyArr tyT11 tyT12 -> if tyT2 == tyT11
            then tyT12
            else error "parameter type mismatch"
          _ -> error "arrow type expected"

getTypeFromContext ctx i = ctx !! i
addbinding ctx x ty = (fst (getTypeFromContext ctx x) ++ "'",ty) : ctx

main = print $ typeOf (TmIf (TmApp (TmApp (TmAbs "a" (TmVar 0 2)) (TmAbs "b" (TmVar 0 2))) (TmApp (TmAbs "a" (TmVar 0 2)) (TmAbs "b" (TmVar 0 2)))))
