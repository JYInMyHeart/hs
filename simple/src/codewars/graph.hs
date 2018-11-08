type Node = Char

type Arc = (Node, Node)

solveGraph :: Node -> Node -> [Arc] -> Bool
solveGraph s e arcs =
  case dfs s e arcs [s] of
    Just a -> True
    Nothing -> False

dfs :: Node -> Node -> [Arc] -> [Node] -> Maybe [Node]
dfs s e g b
  | s == e = Just []
  | s `elem` drop 1 b = Nothing
  | otherwise = (s :) <$> dfs (go s g (length g - 1)) e g (s : b)

--   | (head b, s) `elem` g = Nothing
go x y n
  | n == -1 = x
  | x == fst ((!!) y n) = snd ((!!) y n)
  | otherwise = go x y (n - 1)

ex x y = any (\(u, v) -> x == u && ex v y) y

-- main =
--   print $
--   solveGraph
--     'a'
--     'd'
--     [('a', 'b'), ('b', 'c'), ('c', 'a'), ('c', 'd'), ('e', 'a')]
-- main =
--   print $ gg 'a' [('a', 'b'), ('b', 'c'), ('c', 'a'), ('c', 'd'), ('e', 'a')] 4
-- foldr.flip
main =
  print $ ex 'a' [('a', 'b'), ('b', 'c'), ('c', 'a'), ('c', 'd'), ('e', 'a')]
