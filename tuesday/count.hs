import Data.List
------------ Exercises 3.2.1 & 3.2.2 --------------------
checkLength :: [a] -> Int
checkLength [] = 0
checkLength (_:xs) = 1 + checkLength xs
---------------- Exercise 3.2.3 -------------------------
meanList :: [Int] -> Double
meanList [] = 0
meanList xs = (fromIntegral (sum xs)) / (fromIntegral (length xs))
---------------- Exercise 3.2.4 -------------------------
generatePalindrome :: [a] -> [a]
generatePalindrome [] = []
generatePalindrome xs = xs ++ reverse xs
---------------- Exercise 3.2.5 -------------------------
checkPalindrome :: Eq a => [a] -> Bool
checkPalindrome xs | null xs = False
checkPalindrome xs | xs == reverse xs = True
checkPalindrome xs | xs /= reverse xs = False
---------------- Exercise 3.2.6 -------------------------
orderByListLength :: [[a]] -> [[a]]
orderByListLength xs = sortBy listLength xs
  where listLength a b  | length a <= length b = LT
                        | otherwise = GT
---------------- Exercise 3.2.7 -------------------------
myIntersperse :: a -> [[a]] -> [a]
myIntersperse p (x:y:xs) | null xs = x ++ [p] ++ y
myIntersperse p (x:y:xs) | null x /= True = x ++ [p] ++ y ++ [p] ++ myIntersperse p xs

-- myIntersperse p xs = intercalate p xs
