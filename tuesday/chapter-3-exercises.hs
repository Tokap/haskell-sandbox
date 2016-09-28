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
meanList :: [Int] -> Double
meanList [] = 0
meanList xs = (fromIntegral (sum xs)) / (fromIntegral (length xs))

---------------- Exercise 3.2.4 (Closing Excercise Box) -------------------------
-- Write a function that turns a list into a palindrome.
-- Actual function in count.hs file for testing purposes. Results look like:
generatePalindrome :: [a] -> [a]
generatePalindrome [] = []
generatePalindrome xs = xs ++ reverse xs

---------------- Exercise 3.2.5 (Closing Excercise Box) -------------------------
-- Write a function that checks if a list is a palindrome.
-- Actual function in count.hs file for testing purposes. Results look like:
checkPalindrome :: Eq a => [a] -> Bool
checkPalindrome xs | null xs = False
checkPalindrome xs | xs == reverse xs = True
checkPalindrome xs | xs /= reverse xs = False

---------------- Exercise 3.2.6 (Closing Excercise Box) -------------------------
-- Create a function that sorts a list of lists based on the length of each sublist.
-- Ascending vs Descending order was not specified. Used Asc.
-- TEST DATA: let x = [[1,2,3],[5,6,7,8,9],[4]]
orderByListLength :: [[a]] -> [[a]]
orderByListLength xs = sortBy listLength xs
  where listLength a b  | length a <= length b = LT
                        | otherwise = GT

---------------- Exercise 3.2.7 (Closing Excercise Box) -------------------------
myIntersperse :: a -> [[a]] -> [a]
myIntersperse p (x:y:xs) | null xs = x ++ [p] ++ y
myIntersperse p (x:y:xs) | null x /= True = x ++ [p] ++ y ++ [p] ++ myIntersperse p xs

---------------- Exercise 3.2.8 (Closing Excercise Box) -------------------------
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
-- Using this particular version of Tree creation, we gain access to
-- 'Empty' in order to check against that scenario.
treeHeightCheck :: Tree a -> Int
treeHeightCheck Empty = 0
treeHeightCheck (Node _ ft st) = 1 + max (treeHeightCheck ft) (treeHeightCheck st)

---------------- Exercise 3.2.9 (Closing Excercise Box) -------------------------
