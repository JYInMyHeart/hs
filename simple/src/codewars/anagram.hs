import Data.List

anagrams :: String -> [String] -> [String]
anagrams w = filter (\x -> x \\ w == "")
