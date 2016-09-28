---------------- Exercise 4.1.1  -------------------------
safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _ = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail _ = Nothing

safeLast :: [a] -> Maybe a
safeLast (_:xs) = Just (xs !! (length xs - 1))
safeLast _ = Nothing

-- The below function is more efficient as it doesnt call on length
altLast :: [a] -> Maybe a
altLast (x:xs) | null xs = Just x
altLast (x:xs) | not (null xs) = altLast xs
altLast _ = Nothing

-- safeInit :: [a] -> Maybe [a]
