-- Return the greatest subarray sum within the array of integers passed in.
maxSequence :: [Int] -> Int
maxSequence = maximum . scanl (\acc x -> max 0 acc + x) 0
-- def height(n, m):
-- h, t = 0, 1
-- for i in range(1, n + 1): 
--     t = t * (m - i + 1) // i
--     h += t
-- return h
