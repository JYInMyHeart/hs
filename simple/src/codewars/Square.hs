isSquare :: Integral n => n -> Bool
isSquare n =
  let sr = round $ sqrt $ fromIntegral n
  in sr * sr == n

main = print $ 2
