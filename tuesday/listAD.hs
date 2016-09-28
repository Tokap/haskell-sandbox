data List a = Cons a (List a)
            | Nil
            deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

-- --fromList [Just True, Nothing, Just False]
-- -- => Cons (Just True) (Cons Nothing (Cons (Just False) Nil))

converseFromList (Cons x xs) = x : converseFromList xs
converseFromList Nil = []

-- TEST INSTANCE:
-- converseFromList (Cons (Just True) (Cons Nothing (Cons (Just False) Nil)))
