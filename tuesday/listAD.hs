data List a = Cons a (List a)
            | Nil
            deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

-- --fromList [Just True, Nothing, Just False]
-- -- => Cons (Just True) (Cons Nothing (Cons (Just False) Nil))

converseList (Cons x xs) = x : converseList xs
converseList Nil = []

-- TEST INSTANCE:
-- converseList (Cons (Just True) (Cons Nothing (Cons (Just False) Nil)))
