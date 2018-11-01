alphabetWar :: String -> String
alphabetWar "" = "Let's fight again"
alphabetWar s
  | res > 0 = "Left side wins"
  | res < 0 = "Right side wins"
  | otherwise = "Let's fight again"
  where res = sum $ map powers s


powers :: Char -> Int
powers 'w' = 4
powers 'p' = 3
powers 'b' = 2
powers 's' = 1
powers 'm' = -4
powers 'q' = -3
powers 'd' = -2
powers 'z' = -1
powers _   = 0
