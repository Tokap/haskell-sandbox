checkLength :: [a] -> Int
checkLength [] = 0
checkLength (_:xs) = 1 + checkLength xs
