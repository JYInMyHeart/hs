{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where
import qualified Data.Set as S
import Control.Applicative




main = print $ Jsut [(+1),(+5)] <<*>> Just [1,3]

newtype Velocity = Velocity Int deriving (Num,Eq)

instance Show Velocity where
    show (Velocity n) = show n ++ "m/s"


data MyNum = O | Zero | One

instance Eq MyNum where
    O == Zero = True
    O == O = True
    Zero == Zero = True
    One == One = True
    _ == _ = False


data Tree a = Leaf a | Branch (Tree (a,a)) deriving Show


(<<*>>) :: f1 (f2 (a -> b)) -> f1 (f2 a) -> f1 (f2 b)
(<<*>>) = liftA2 (<*>)







