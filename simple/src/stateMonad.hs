import           Control.Monad.State

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show,Eq)

labelTree :: Tree a -> Tree (a,Int)
labelTree t = fst $ ntAux t 0

ntAux :: Tree a -> Int -> (Tree (a,Int),Int)
ntAux (Leaf a) n = (Leaf (a,n),n + 1)
ntAux (Node l a r) n = let (nn,n') = ((a,n),n+1) in
  let(ln,n'') = ntAux l n' in
    let(rn,n''') = ntAux r n'' in
      (Node ln nn rn,n''')

test :: Tree Int
test = Node(Node (Leaf 5) 3 (Leaf 2)) 7 (Leaf 9)

increase :: State Int Int
increase = state $ \i -> (i,i + 1)

ntAux' :: Tree a -> State Int(Tree (a,Int))
ntAux' (Leaf a) = do
  nl <- increase
  return (Leaf (a,nl))
ntAux' (Node l n r) = do
  n1 <- increase
  lt <- ntAux' l
  rt <- ntAux' r
  return (Node lt (n,n1) rt)
labelTree' t = evalState (ntAux' t) 0



type Stack = [Int]
pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

peek :: State Stack Int
peek = state $ \(x:xs) -> (x,x:xs)

push :: Int -> State Stack ()
push i = state $ \xs -> ((),i : xs)

test2 = runState (push 5 >>= const pop) [0]
-- test3 = runState (push 4 >>= const pop) test2

addStack :: State Stack ()
addStack = do
  a1 <- pop
  a2 <- pop
  let a3 = a1 + a2
  push a3

main = print $  runState  addStack [1,2]
