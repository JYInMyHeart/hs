{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Bits             (shiftL)
import qualified Data.ByteString       as DB
import qualified Data.ByteString.Char8 as DBC (putStrLn)
import           Data.List             (foldl')
import           Data.String           (fromString)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as Encode (decodeUtf8)
import           Data.Word             (Word8)
import           System.Environment    (getArgs)
import           System.IO
import           Text.Printf



data WordIdx = WordIndex
    {
        word    :: T.Text
       , offset :: Int
       , expLen :: Int
    } deriving Show
searchWord :: T.Text -> [WordIdx] -> Maybe WordIdx
searchWord str [] = Nothing
searchWord str xs
    | wrd < str = searchWord str behind
    | wrd > str = searchWord str front
    | otherwise = Just b
    where (front,b:behind) = splitAt (length xs `div` 2) xs
          wrd = T.toLower (word b)
