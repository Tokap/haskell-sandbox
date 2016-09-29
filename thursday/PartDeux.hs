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
asInt_fold ('-':xs) = -(asInt_fold xs) -- check for negative symbol
asInt_fold xs = foldl (\acc x -> (acc * 10) + digitToInt x) 0 xs


-- Below is simplest conversion method, but is definitely impure:
-- asInt_fold :: String -> Int
-- asInt_fold xs = read xs :: Int

-- A PIECE OF EXAMPLE CODE FROM "LEARN ME A GOOD...":
-- -- Clever Sum
-- sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\acc x -> acc + x) 0 xs
