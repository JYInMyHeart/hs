import           Control.Monad.State
import           Control.Monad.Trans.Maybe

pushSM :: Int -> StateT [Int] Maybe ()
pushSM x = StateT $ \xs -> Just ((),x:xs)

popSM :: StateT [Int] Maybe Int
popSM = StateT $ \xs -> case xs of
  []     -> Nothing
  (x:xs) -> Just  (x,xs)


main = print $ runStateT ( pushSM 1) []
