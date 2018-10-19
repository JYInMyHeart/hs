import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as T

word :: Parsec String () String
word = many1 letter

-- main = do
--   a <- parseTest word "hello world"
--   print a
test1 = do
  a <- parseTest float "1.2"
  print a

main = test2

lexer :: T.TokenParser ()
lexer = T.makeTokenParser emptyDef

lexeme :: Parsec String () a -> Parsec String () a
lexeme = T.lexeme lexer

float :: Parsec String () Double
float = T.float lexer

float1 :: Parsec String () Double
float1 = do
  f <- lexeme sign
  n <- T.float lexer
  return (f n)

sign :: Num a => Parsec String () (a -> a)
sign = (char '-' >> return negate) <|> (char '+' >> return id) <|> return id

test2 = do
  a <- parseTest float1 "-1.2"
  print a
