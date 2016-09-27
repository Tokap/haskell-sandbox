checkLength :: [a] -> Int
checkLength [] = 0
checkLength (_:xs) = 1 + checkLength xs

meanList :: [Int] -> Double
meanList [] = 0
meanList xs = (fromIntegral (sum xs)) / (fromIntegral (length xs))

makePalindrome :: [a] -> [a]
makePalindrome [] = []
makePalindrome xs = xs ++ reverse xs
