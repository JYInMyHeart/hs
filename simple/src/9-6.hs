{-# LANGUAGE MultiParamTypeClasses,TypeFamilies #-}
class (Num a,Num b) => GPlus a b where
    type SumType a b :: *
    plus :: a -> b -> SumType a b

instance GPlus Int Int where
    type SumType Int Int = Int

instance GPlus Int Int where
    type SumType Int Int = Float

