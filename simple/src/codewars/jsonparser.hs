

import           Control.Applicative hiding (many)
import           Control.Monad
import           Data.Char


parse :: String -> Maybe Value
parse s = parseCode  value s
a = sat (/= '-')
main = print $ parseCode value "{\"menu\":{\"header\":\"SVG Viewer\",\"items\":[{\"id\":\"Open\"},{\"id\":\"OpenNew\",\"label\":\"Open New\"},null,{\"id\":\"ZoomIn\",\"label\":\"Zoom In\"},{\"id\":\"ZoomOut\",\"label\":\"Zoom Out\"},{\"id\":\"OriginalView\",\"label\":\"Original View\"},null,{\"id\":\"Quality\"},{\"id\":\"Pause\"},{\"id\":\"Mute\"},null,{\"id\":\"Find\",\"label\":\"Find...\"},{\"id\":\"FindAgain\",\"label\":\"Find Again\"},{\"id\":\"Copy\"},{\"id\":\"CopyAgain\",\"label\":\"Copy Again\"},{\"id\":\"CopySVG\",\"label\":\"Copy SVG\"},{\"id\":\"ViewSVG\",\"label\":\"View SVG\"},{\"id\":\"ViewSource\",\"label\":\"View Source\"},{\"id\":\"SaveAs\",\"label\":\"Save As\"},null,{\"id\":\"Help\"},{\"id\":\"About\",\"label\":\"About Adobe CVG Viewer...\"}]}"
-- main = print $ parse "{\"ab\":[123,3]}"

data Value = String String
           | Number Double
           | Object [(Value,Value)] -- an association list -- only a `String` is valid as the index `Value`
           | Array [Value]          -- not limited to identical primitive datatypes
           | Boolean Bool           -- either `True` or `False`
           | Null deriving (Show)



parseObject = (do
  symb "{"
  m <- members
  symb "}"
  return $ Object m) <|> do
    symb "{"
    t <- return []
    symb "}"
    return $ Object t

true = do
  symb "true"
  return $ Boolean True
false = do
  symb "false"
  return $ Boolean False
nu = do
  symb "null"
  return Null
array = (do
  symb "["
  a1 <- return []
  a <- value
  b <- many values
  symb "]"
  return $ Array $ a1 ++ [a] ++ b)   <|> do
     symb "["
     a <- return []
     symb "]"
     return $ Array a

values = do
  symb ","
  value
str  =  do
  symb "\""
  cs <- many any'
  symb "\""
  return $ String  cs
any' = sat (`elem` something)

something = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "!@#$%^&*()~!+_ '\\"
value =  frac <|> nu <|> array <|> false <|> true <|> parseObject <|> str
pair = do
  a <- str
  symb ":"
  b <- value
  return $ (a,b)

members = do
  s <- return []
  cs <-  pair
  cs1 <- many pair'
  return $ s ++ [cs] ++ cs1


pair' = do
  symb ","
  a <- pair
  return a

newtype Parser a = Parser
  { parses :: String -> [(a, String)]
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
  p >>= f = Parser (\cs -> concat [parses (f a) cs' | (a, cs') <- parses p cs])

instance MonadPlus Parser where
  mzero = Parser $ const []
  mplus p q = Parser $ \s -> parses p s ++ parses q s

instance Alternative Parser where
  empty = mzero
  p <|> q =
    Parser $ \s ->
      case parses p s of
        [] -> parses q s
        rs -> rs

item :: Parser Char
item =
  Parser
    (\cs ->
       case cs of
         ""     -> []
         (c:cs) -> [(c, cs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c
    then return c
    else mzero

char :: Char -> Parser Char
char c = space >> sat (c ==)

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
  parses
    (do space
        p)

parseCode :: Parser a -> String -> Maybe a
parseCode p s =
  case apply p s of
    [(res, [])] -> Just res
    _           -> Nothing
digit = sat isDigit
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $  s ++ cs

frac =  (do
    a <- number
    symb "."
    b <- number
    return $ Number $ read $ a ++ "." ++ b) <|> do
      c <- number
      return $ Number $ read  c
