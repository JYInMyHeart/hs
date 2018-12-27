module Context where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Text.PrettyPrint as PP

data Exp = EVar String
  | ELit Lit
  | EApp Exp Exp
  | EAbs String Exp
  | ELet String Exp Exp
  deriving (Eq,Ord)

data Lit = LInt Integer
  | LBool Bool
  deriving (Eq,Ord)

data Type = TVar String
  | TInt
  | TBool
  | TFun Type Type
  deriving(Eq,Ord)

data Scheme = Scheme [String] Type

class Types a where
  ftv :: a -> Set.Set String
  apply :: Subst -> a -> a

instance Types Type where
  ftv(TVar n) = Set.fromList [n]
  ftv TInt = Set.empty
  ftv TBool = Set.empty
  ftv (TFun t1 t2) = ftv t1 `Set.union` ftv t2
  apply s (TVar n) = case Map.lookup n s of
                        Nothing -> TVar n
                        Just t -> t
  apply s (TFun t1 t2) = TFun (apply s t1)(apply s t2)
  apply s t = t

instance Types Scheme where
    ftv (Scheme vars t) = (ftv t) Set.\\ (Set.fromList vars)
    apply s (Scheme  vars t) = Scheme vars (apply (foldr Map.delete s vars )t)

instance Types a => Types [a] where
    apply s  = map(apply s)
    ftv l = foldr Set.union Set.empty (map ftv l)

type Subst = Map.Map String Type
nullSubst :: Subst
nullSubst = Map.empty
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = (Map.map (apply s1) s2) `Map.union` s1

newtype TypeEnv = TypeEnv(Map.Map String Scheme)

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

instance Types TypeEnv where
  ftv(TypeEnv env)  = ftv (Map.elems env)
  apply s (TypeEnv env) = TypeEnv(Map.map (apply s) env)

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
  where vars = Set.toList ((ftv t) Set.\\ (ftv env))

