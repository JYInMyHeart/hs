import Control.Monad

evalSeq :: Maybe Integer -> (Integer -> Maybe Integer) -> Maybe Integer
evalSeq mi f = case mi of
  Nothing -> Nothing
  Just a  -> f a

main = print $ (\ b -> Just (b + 1)) 5
