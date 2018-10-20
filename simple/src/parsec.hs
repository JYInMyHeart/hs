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

test3 = do
  a <- parseTest parseExp "1.0 + 2.0 * 4.0 +3.9 "
  print a

test4 = print $ calculate "1.0 + 2.0  * 4.0+3.9 "

main = do
  test3
  test4

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

eval' :: Parsec String () Double
eval' = do
  e1 <- parseExp
  return $ eval e1

-- calculate :: String -> IO ()
-- calculate e = parseTest eval' e
calculate str =
  case runParser parseExp () "" str of
    Right exp -> eval exp
    Left _ -> error "error"

--Exp ::== Mul Exp'
parseExp :: Parsec String () Exp
parseExp = do
  e1 <- parseMul
  e2 <- parseExp'
  case e2 of
    Nothing -> return e1
    Just e -> return (e e1)
-- Exp' ::== + Mul Exp' | ""
parseExp' :: Parsec String () (Maybe (Exp -> Exp))
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
parseMul :: Parsec String () Exp
parseMul = do
  e1 <- parseNum
  e2 <- parseMul'
  case e2 of
    Nothing -> return e1
    Just e -> return (e e1)
-- Mul' ::== * Num Mul' | ""
parseMul' :: Parsec String () (Maybe (Exp -> Exp))
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
parseNum :: Parsec String () Exp
parseNum =
  try
    (do char '('
        e1 <- parseExp
        char ')'
        return e1) <|>
  (do num <- float1
      return (Val num))
