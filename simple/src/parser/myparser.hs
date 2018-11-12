import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char

newtype Parser a = Parser
  { parse :: String -> [(a, String)]
  }

instance Functor Parser where
  fmap f (Parser ps) = Parser $ \p -> [(f a, b) | (a, b) <- ps p]

--
instance Applicative Parser where
  pure = return
  (Parser p1) <*> (Parser p2) =
    Parser $ \p -> [(f a, s2) | (f, s1) <- p1 p, (a, s2) <- p2 s1]

instance Monad Parser where
  return a = Parser (\cs -> [(a, cs)])
  p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs])

instance MonadPlus Parser where
  mzero = Parser $ const []
  mplus p q = Parser $ \s -> parse p s ++ parse q s

instance Alternative Parser where
  empty = mzero
  p <|> q =
    Parser $ \s ->
      case parse p s of
        [] -> parse q s
        rs -> rs

item :: Parser Char
item =
  Parser
    (\cs ->
       case cs of
         "" -> []
         (c:cs) -> [(c, cs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c
    then return c
    else mzero

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do
  char c
  string cs
  return (c : cs)

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do
  a <- p
  as <- many p
  return (a : as)

-- | 1+
-- some v = someV
--   where
--     manyV = someV <|> pure []
--     someV = (:) <$> v <*> manyV
-- --
-- -- | 0+
-- many v = manyV
--   where
--     manyV = someV <|> pure []
--     someV = (:) <$> v <*> manyV
sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) <|> return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do
  a <- p
  as <-
    many
      (do sep
          p)
  return (a : as)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
  a <- p
  rest a
  where
    rest a =
      (do f <- op
          b <- p
          rest (f a b)) <|>
      return a

space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = do
  a <- p
  space
  return a

symb :: String -> Parser String
symb = token . string

apply :: Parser a -> String -> [(a, String)]
apply p =
  parse
    (do space
        p)

parseCode :: Parser a -> String -> a
parseCode p s =
  case apply p s of
    [(res, [])] -> res
    _ -> error "Huge"

expr :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)
expr = term `chainl1` addop

term = factor `chainl1` mulop

factor =
  number <|> do
    symb "("
    n <- expr
    symb ")"
    return n

digit = token $ sat isDigit

number = do
  s <- string "-" <|> return []
  cs <- many1 digit
  return $ read $ s ++ cs

addop =
  (do symb "+"
      return (+)) <|>
  (do symb "-"
      return (-))

mulop =
  (do symb "*"
      return (*)) <|>
  (do symb "/"
      return div)


main = print $ parseCode expr " 1 + 11 + 1 + 1 * 23424 -1 - 1 -2 -3 "
