---------------- Exercise 3.1.1 (First Excercise Box) -------------------------

-- Write the converse of fromList : takes List a and generates a [a]
-- Function written as an addition to listAD.hs

---------------- Exercise 3.1.2 (First Excercise Box) -------------------------
-- Define a tree type that has only one constructor.

-- Using Maybe makes that part of the definition optional.

-- Regarding the use of Maybe:
-- "The Maybe type encapsulates an optional value. A value of type Maybe a either
-- contains a value of type a (represented as Just a), or it is empty (represented
-- as Nothing). Using Maybe is a good way to deal with errors or exceptional cases
-- without resorting to drastic measures such as error."
-- https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Maybe.html

data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)
