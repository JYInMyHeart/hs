{-# LANGUAGE Strict #-}
module RBT where


--��ɫ
data Color = R|B
    deriving(Show,Eq)
data RBT a = Empty
    | MkRBT Color (RBT a) a (RBT a)
    deriving(Show,Eq)
--without delete method
--the height of brt with n node is smaller than 2*lg(n+1)

--balance
-- return
--    R
-- B     B

rbtBalance :: Color -> RBT a -> a -> RBT a -> RBT a

--    B
--  R
--R
----------------------
-- x y z is root
rbtBalance B (MkRBT R (MkRBT R a x b) y c) z d =
  MkRBT R (MkRBT B a x b) y (MkRBT B c z d)
-- B
--   R
--     R
----------------------
-- x y z is root
rbtBalance B a x (MkRBT R b y (MkRBT R c z d)) =
  MkRBT R (MkRBT B a x b) y (MkRBT B c z d)
--    B
--  R
--   R
----------------------
-- x y z is root
rbtBalance B (MkRBT R a x (MkRBT R b y c)) z d =
  MkRBT R (MkRBT B a x b) y (MkRBT B c z d)
-- B
--   R
-- R
----------------------
-- x y z is root
rbtBalance B a x (MkRBT R (MkRBT R b y c) z d) =
  MkRBT R (MkRBT B a x b) y (MkRBT B c z d)

-- do nothing if tree is balanced
rbtBalance c a x b = MkRBT c a x b

rbtmkBlack :: RBT a -> RBT a
rbtmkBlack Empty           = Empty
rbtmkBlack (MkRBT _ a x b) = MkRBT B a x b

rbtInsert :: Ord a => RBT a -> a -> RBT a
rbtInsert t x = rbtmkBlack $ ins t
 where
  ins Empty = MkRBT R Empty x Empty
  ins (MkRBT c l k r) | x < k     = rbtBalance c (ins l) k r
                      | otherwise = rbtBalance c l k (ins r)

rbtInsertList :: Ord a => RBT a -> [a] -> RBT a
rbtInsertList Empty []       = Empty
rbtInsertList Empty (x : xs) = rbtInsertList pre xs
  where pre = MkRBT B Empty x Empty
rbtInsertList t []       = t
rbtInsertList t (y : ys) = rbtInsertList pre ys where pre = rbtInsert t y
--look up
rbtSearch :: Ord a => RBT a -> a -> Bool
rbtSearch Empty _ = False
rbtSearch (MkRBT _ a x b) k | k == x    = True
                            | k < x     = rbtSearch a k
                            | otherwise = rbtSearch b k
