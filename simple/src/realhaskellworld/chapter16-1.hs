import Text.ParserCombinators.Parsec

csvFile = endBy line eol

line = sepBy cell (char ',')

cell = many (noneOf ",\n")

eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"

main = print $ parseCSV "hi\n1,2,3,4,5,6\n"
