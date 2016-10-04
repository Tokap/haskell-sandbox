import Data.List

-- traverese a list and find the index where:
-- sum leftSide == sum rightSide (index acts as pivot)
-- return (-1) if nothing matches
traverseList :: Int -> [Int] -> Int
traverseList index [] = (-1)
traverseList index xs | index == (fromIntegral (length xs)) = (-1)
traverseList index xs | index < length xs                   = if leftHalf == rightHalf
                                                               then index
                                                               else traverseList (index + 1) xs
                                                           where numSplit  = splitAt index xs
                                                                 leftHalf  =  ( sum (init ( fst numSplit ) ) )
                                                                 rightHalf =  ( sum (snd numSplit) )


findEvenIndex :: [Int] -> Int
findEvenIndex [] = (-1)
findEvenIndex xs = traverseList 1 xs
---------------------------------------------------------------------------------------------------------------------
-- Your task is to make a function that can take any non-negative integer as a
-- argument and return it with it's digits in descending order. Descending
-- order means that you take the highest digit and place the next highest
-- digit immediately after it.
--
-- Examples:
-- Input: 145263 Output: 654321
-- Input: 1254859723 Output: 9875543221
descendingOrder :: Integer -> Integer
descendingOrder n | n < 0 = 0
descendingOrder n | otherwise = read (reverse $ sort (show n)) :: Integer

----- REFINED ANSWER:
-- descendingOrder :: Integer -> Integer
-- descendingOrder = read . reverse . sort . show
---------------------------------------------------------------------------------------------------------------------
-- named with letters from a to m.
-- s="aaabbbbhaijjjm"
-- error_printer(s) => "0/14"
--
-- s="aaaxbbbbyyhwawiwjjjwwm"
-- error_printer(s) => "8/22"

printerError :: [Char] -> [Char]
printerError s = concat returnValue
    where total = show $ length s
          alpha = ['a'..'m']
          theSplit = partition (`elem` alpha) s
          glitchMatch = show $ length (snd theSplit)
          returnValue = glitchMatch : "/" : [total]
