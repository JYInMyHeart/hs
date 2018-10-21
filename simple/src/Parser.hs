module Parser where

import Control.Applicative
import Data.Char

type Line = Int

type Column = Int

data Pos = Pos
  { getLine :: Line
  , getColumn :: Column
  } deriving (Eq, Show)

updatePos :: Pos -> Char -> Pos
updatePos (Pos l c) char =
  case char of
    '\n' -> Pos (l + 1) 1
    '\t' -> Pos 1 ((c + 8 - (c - 1) `mod` 8))
    _ -> Pos l (c + 1)

initialPos :: Pos
initialPos = Pos 1 1

data State s = State
  { stateInput :: s
  , statePos :: Pos
  } deriving (Eq, Show)

data Reply s a
  = Ok a
       (State s)
       ParseError
  | Error ParseError
  deriving (Eq, Show)

data Message
  = Info String
  | Warn String
  | Err String
  deriving (Eq, Show)

data ParseError =
  ParseError [Message]
  deriving (Eq, Show)

appendError :: ParseError -> Message -> ParseError
appendError (ParseError a) msg = ParseError (msg : a)

mergeError :: ParseError -> ParseError -> ParseError
mergeError (ParseError a) (ParseError b) = ParseError (a ++ b)

data Consumed a
  = Consumed a
  | Empty a

data Parser s a = Parser
  { runParser :: State s -> ParseError -> Consumed (Reply s a)
  }

instance Functor (Parser s) where
  fmap f p =
    Parser $ \st error ->
      case runParser p st error of
        Consumed (Ok r st' err) -> Consumed (Ok (f r) st' err)
        Consumed (Error err) -> Consumed (Error err)
        Empty (Ok r st' err) -> Empty (Ok (f r) st' err)
        Empty (Error err) -> Empty (Error err)

instance Monad (Parser s) where
  return inp = Parser $ \st error -> Empty (Ok inp st error)
  p >>= f =
    Parser $ \st error ->
      case runParser p st error of
        Consumed (Ok r st' err) -> runParser (f r) st' err
        Consumed (Error err) -> Consumed (Error err)
        Empty (Ok r st' err) -> runParser (f r) st' err
        Empty (Error err) -> Empty (Error err)

instance Applicative (Parser s) where
  pure = return
  (<*>) pf pa = do
    f <- pf
    m <- pa
    return $ f m

instance Alternative (Parser s) where
  empty = Parser $ \st err -> Empty (Error err)
  p <|> q =
    Parser $ \st err ->
      case runParser p st err of
        Empty (Error err') -> runParser q st err
        Empty o@(Ok r st' err') ->
          case runParser q st err of
            Empty _ -> Empty o
            consumed -> consumed
        consumed -> consumed

try :: Parser String a -> Parser String a
try p =
  Parser $ \input err ->
    case (runParser p input err) of
      Consumed (Error err') -> Empty (Error err')
      result -> result

(<?>) :: Parser s a -> Message -> Parser s a
(<?>) p msg =
  Parser $ \st err ->
    case runParser p st err of
      Empty (Error err') -> Empty (Error (appendError err' msg))
      Consumed (Error err') -> Consumed (Error (appendError err' msg))
      result -> result

satisfy :: (Char -> Bool) -> Parser String Char
satisfy f =
  Parser $ \(State str pos) err ->
    case str of
      c:cs ->
        if f c
          then Consumed (Ok c (State cs (updatePos pos c)) err)
          else Empty (Error (ParseError [Err ("error at " ++ show pos)]))
      [] ->
        Empty
          (Error
             (ParseError [Err ("error at " ++ show pos ++ " input exausted.")]))

char :: Char -> Parser String Char
char c = satisfy (== c) <?> Info ("expect a character " ++ show c)

number :: Parser String Char
number = satisfy isDigit <?> Info ("expect a character ")

letter :: Parser String Char
letter = satisfy isAlpha <?> Info "expect an alpha"

string :: String -> Parser String String
string [] = return []
string (s:str) = do
  c <- char s
  cs <- string str
  return (c : cs)

parse :: String -> Parser String a -> a
parse str p =
  case runParser p (State str initialPos) (ParseError []) of
    Consumed (Ok r st' err) -> r
    Consumed (Error err) -> error $ show err
    Empty (Ok r st err) -> r
    Empty (Error err) -> error $ show err

data Exp
  = Add Exp
        Exp
  | Mul Exp
        Exp
  | Val Double
  deriving (Eq, Show)

eval (Val v) = v
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

eval' :: Parser String Double
eval' = do
  e1 <- parseExp
  return $ eval e1

--Exp ::== Mul Exp'
parseExp :: Parser String Exp
parseExp = do
  e1 <- parseMul
  e2 <- parseExp'
  case e2 of
    Nothing -> return e1
    Just e -> return (e e1)

-- Exp' ::== + Mul Exp' | ""
parseExp' :: Parser String (Maybe (Exp -> Exp))
parseExp' =
  try
    (do char '+'
        e1 <- parseMul
        e2 <- parseExp'
        case e2 of
          Nothing -> return (Just (\e -> Add e e1))
          Just e -> return (Just (\e' -> e (Add e' e1)))) <|>
  return Nothing

-- Mul ::== Num Mul'
parseMul :: Parser String Exp
parseMul = do
  e1 <- parseNum
  e2 <- parseMul'
  case e2 of
    Nothing -> return e1
    Just e -> return (e e1)

-- Mul' ::== * Num Mul' | ""
parseMul' :: Parser String (Maybe (Exp -> Exp))
parseMul' =
  try
    (do char '*'
        e1 <- parseNum
        e2 <- parseMul'
        case e2 of
          Nothing -> return (Just (\e -> Mul e e1))
          Just e -> return (Just (\e' -> e (Mul e' e1)))) <|>
  return Nothing

-- Num ::== (Exp) | Number
parseNum :: Parser String Exp
parseNum =
  try
    (do char '('
        e1 <- parseExp
        char ')'
        return e1) <|> do
    num <- number
    return (Val (read [num]))

main = print $ eval $ parse "1+3" parseExp
-- main = print $ parse "(1)" parseNum
