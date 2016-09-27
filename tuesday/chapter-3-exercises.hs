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

---------------- Exercise 3.2.1 (Closing Excercise Box) -------------------------
-- Write a funciton to count elements in a list. Should result the same as length.
-- Actual function in count.hs file for testing purposes. Results look like:
checkLength [] = 0
checkLength (_:xs) = 1 + checkLength xs

---------------- Exercise 3.2.2 (Closing Excercise Box) -------------------------
-- Add a type signature to my counting function
-- Added the below line of code to the top of the count.hs file.
checkLength :: [a] -> Int

---------------- Exercise 3.2.3 (Closing Excercise Box) -------------------------
-- Write a function that calculates the mean of a list.
-- Actual function in count.hs file for testing purposes. Results look like:
meanList :: [Int] -> Int
meanList [] = 0
meanList xs = sum xs `div` length xs
