module Parser where

import Control.Applicative
import Data.Char

type Line = Int
type Column = Int

data Pos = Pos{getLine :: Line,
               getColumn :: Column} deriving (Eq,Show)

updatePos :: Pos -> Char -> Pos
updatePos (Pos l c) char = 
    case char of
    '\n' -> Pos (l + 1) 1
    '\t' -> Pos 1 ((c + 8 - (c - 1) `mod` 8))
    _    -> Pos l (c + 1)

initialPos :: Pos
initialPos = Pos 1 1

data State s = State{stateInput :: s.statePos :: Pos} deriving (Eq,Show)

data Reply s a = OK a (State s) ParseError | Error ParseError deriving (Eq,Show)

data Message = Info String | Warn String | Err String deriving (Eq,Show)

data ParseError = ParseError [Message] deriving (Eq,Show)

appendError :: ParseError -> Message -> ParseError
appendError (ParseError a) msg = ParseError (msg : a)

data Consumed a = Consumed a | Empty a
data Parser s a = Parser {runParser :: State s -> ParseError -> Consumed (Reply s a)}

instance Functor (Parser s) where
    fmap f p = 
        Parser $ \st error -> case runParser p st error of
            Consumed (Ok r st' err) -> Consumed (Ok (f r) st' err)
            Consumed (Error err) -> Consumed (Error err)
            Empty (Ok r st' err) -> Empty (Ok (f r) st' err)
            Empty (Error err) -> Empty (Error err)

instance Monad (Parser s) where
    return inp = Parser $ \st error -> Empty (Ok inp st error)
    p >>= f =
        Parser $ \st error -> 
            case runParser p st error of
                Consumed (Ok r st' err) -> Consumed (Ok (f r) st' err)
                Consumed (Error err) -> Consumed (Error err)
                Empty (Ok r st' err) -> Empty (Ok (f r) st' err)
                Empty (Error err) -> Empty (Error err)

instance Applicative (Parser s) where
    pure = return
    (<*>) pf pa = do
        f <- pf
        m <- pa
        return $ f m

instance Alternative (Parser s) where
    empty = Parser $ \st -> err -> Empty (Error err)
    p <|> q = 
        Parser $ \st err ->
            case runParser p st err of
                Empty(Error err') -> runParser q st err
                Empty o@(Ok r st' err') ->
                    case runParser q st err of
                        Empty _ -> Empty o
                        consumed -> consumed
                consumed -> consumed
