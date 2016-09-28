---------------- Exercise 4.1.1  -------------------------
safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _ = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail _ = Nothing


-- safeLast :: [a] -> Maybe a
--
--
-- safeInit :: [a] -> Maybe [a]
