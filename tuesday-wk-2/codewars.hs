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
