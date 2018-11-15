


-- true
tru = \t -> \f -> t
-- false
fls = \t -> \f -> f
-- if
test = \l -> \m -> \n -> l m n
-- and
and' = \b -> \c -> b c fls
-- or
or' = \b -> \c -> b tru c
-- pair
pair' = \f s b -> b f s
-- fst
fst' = \p -> p tru
-- snd
snd' = \p -> p fls

plus' = \m n s z -> m s (n s z)

times' = \m n -> m (plus' n) (\s z -> z)
main = print $ and' 1  2 3 4
