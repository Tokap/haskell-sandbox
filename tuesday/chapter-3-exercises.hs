---------------- Exercise 2.1.1 (First Excercise Box) -------------------------

-- Write the converse of fromList : takes List a and generates a [a]
-- basically, reverse input and output.

-- The converse of "If it is raining then the grass is wet" is "If the grass is
-- wet then it is raining." Note: As in the example, a proposition may be true
-- but have a false converse.

-------- ORIGINAL FUNCTION --------
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

--fromList [Just True, Nothing, Just False]
-- => Cons (Just True) (Cons Nothing (Cons (Just False) Nil))
