-- fromList (x:xs) = Cons x (fromList xs)
-- fromList []     = Nil
--
-- --fromList [Just True, Nothing, Just False]
-- -- => Cons (Just True) (Cons Nothing (Cons (Just False) Nil))

converseList Cons (f) (Cons (r)) = f:(converseList r)
converseList (f Cons (r)) = f:(converseList r)
converseList Nil = []
