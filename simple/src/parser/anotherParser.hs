   import Control.Applicative
   import Data.Char

   newtype Parser a = Parser (String -> Maybe (a, String))
    
   parse :: Parser a -> String -> Maybe (a, String)
   parse (Parser parser) = parser

   item :: Parser Char
   item = Parser (\ st -> case st of 
                            [] -> Nothing
                            (x : xs) -> Just (x , xs))

   instance Functor Parser where
        -- fmap :: (a -> b) -> Parser a -> Parser b
        fmap f p = Parser (\ st -> case parse p st of
                                        Nothing -> Nothing
                                        Just (v, out) -> Just (f v, out))

    instance Applicative Parser where

        -- pure :: a -> Parser a
        pure v = Parser (\ st -> Just (v, st))

        -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
        pab <*> pa = Parser (\ st -> case parse pab st of
            Nothing -> Nothing
            Just (v, out) -> parse (fmap v pa) out)

    instance Monad Parser where

        -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
        pa >>= apb = Parser (\ st -> case parse pa st of
            Nothing -> Nothing
            Just (a, out) -> parse (apb a) out)

    instance Alternative Parser where

        -- empty :: Parser a
        empty = Parser $ const Nothing

        -- <|> :: Parser a -> Parser a -> Parser a
        pa <|> pa2 = Parser (\ st -> case parse pa st of
            Nothing -> parse pa2 st
            Just x -> Just x)

    data RegExp = Normal Char       -- ^ A character that is not in "()*|."
        | Any               -- ^ Any charater
        | ZeroOrMore RegExp -- ^ Zero or more occurances of the same regexp
        | Or RegExp RegExp  -- ^ A choice between 2 regexps
        | Str [RegExp]      -- ^ A sequence of regexps.
        deriving (Show, Eq)

    failure :: Parser a
    failure = Parser $ const Nothing

    sat :: (Char -> Bool) -> Parser Char
    sat f = do 
        x <- item
        if f x then return x else failure

    digit :: Parser Char
    digit = sat isDigit

    lower :: Parser Char
    lower = sat isLower

    upper :: Parser Char
    upper = sat isUpper

    letter :: Parser Char
    letter = sat isLetter

    alphanum :: Parser Char
    alphanum = sat isAlphaNum

    char :: Char -> Parser Char
    char x = sat (==x)

    oneOf :: String -> Parser Char
    oneOf "" = failure
    oneOf [x] = char x
    oneOf (x : xs) = char x <|> oneOf xs

    noneOf :: String -> Parser Char
    noneOf "" = item
    noneOf str = Parser (\ st -> if (st == "") || head st `elem` str then Nothing
        else Just (head st, tail st))

    parseSimple :: Parser RegExp
    parseSimple = do
        c <- noneOf "()*|."
        return $ Normal c
        <|> do
            _ <- char '.'
            return Any
            <|> do
                _ <- char '('
                x <- parseOr
                _ <- char ')'
                return x

    parseStr :: Parser RegExp
    parseStr = do
        x <- parseMulti
        xs <- some parseMulti
        return $ Str (x : xs)
        <|> parseMulti

    parseMulti :: Parser RegExp
    parseMulti = do
        x <- parseSimple
        _ <- char '*'
        return $ ZeroOrMore x
        <|> parseSimple

    parseOr :: Parser RegExp
    parseOr = do
        x <- parseStr
        _ <- char '|'
        y <- parseStr
        return $ Or x y
        <|> parseStr

    parseRegExp :: String -> Maybe RegExp
    parseRegExp s = case parse parseOr s of
        Just (x , []) -> Just x
        _ -> Nothing
