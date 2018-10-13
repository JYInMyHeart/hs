{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where
import Data.Char





data Shape = Circle Double | Square Double | Rectangle Double Double

instance Eq Shape where
    Circle r1 == Circle r2 = r1 == r2
    Square l1 == Square l2 = l1 == l2
    Rectangle a1 b1 == Rectangle a2 b2 = a1 == a2 && b1 == b2
    _ == _ = False

class ToInt a where
    toInt :: a -> Int
instance ToInt Char where
    toInt = ord
newtype C = C Char deriving (Show,ToInt)











main = print $ "1"
-- main = print $ toInt (C 'a')