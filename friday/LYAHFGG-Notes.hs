---------------------------- RECURSION ---------------------------------
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

-- Note: Num is not a subclass of Ord. That means that what constitutes for a 
-- number doesn't really have to adhere to an ordering. So that's why we have to
-- specify both the Num and Ord class constraints when doing addition or
-- subtraction and also comparison.
