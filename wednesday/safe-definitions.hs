---------------- Exercise 4.1.1  -------------------------
safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _ = Nothing


-- safeTail :: [a] -> Maybe [a]
--
--
-- safeLast :: [a] -> Maybe a
--
--
-- safeInit :: [a] -> Maybe [a]
