import Data.Char (digitToInt)

------------- Original Content From Text -----------
asInt :: String -> Int
asInt xs = loop 0 xs

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs
                  -- multiplying by 10 is a cheap way to move the digit over once
-----------------------------------------------------
asInt_fold :: String -> Int
asInt_fold xs | '.' `elem` xs = error "Only potential Ints can be converted"
asInt_fold xs | length xs >= 20 = error "Only numbers under 19 digits in length may be converted"
asInt_fold ('-':xs) = -(asInt_fold xs) -- check for negative symbol
asInt_fold "" = error "Not a possible number" -- handles empty & just "-"
asInt_fold xs | otherwise = foldl (\acc x -> (acc * 10) + digitToInt x) 0 xs

-- Below is simplest conversion method, but is likely impure:
-- asInt_fold :: String -> Int
-- asInt_fold xs = read xs :: Int

---------------- Exercise 4.2.4 -------------------------
-- type ErrorMessage = String
-- asInt_either :: String -> Either ErrorMessage Int
