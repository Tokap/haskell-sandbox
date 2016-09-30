---------------------------- RECURSION ---------------------------------
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

-- Note: Num is not a subclass of Ord. That means that what constitutes for a
-- number doesn't really have to adhere to an ordering. So that's why we have to
-- specify both the Num and Ord class constraints when doing addition or
-- subtraction and also comparison.

---- Quick Sort Implemented In Haskell is Boss ( and the poster child )
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted
--
-- So when trying to think of a recursive way to solve a problem, try to think
-- of when a recursive solution doesn't apply and see if you can use that as an
-- edge case, think about identities and think about whether you'll break apart
-- the parameters of the function (for instance, lists are usually broken into
-- a head and a tail via pattern matching) and on which part you'll use the
-- recursive call.

-------------------------- HIGHER ORDER FUNCTIONS ------------------------------
-- Quick definition & Importance:
-- Haskell functions can take functions as parameters and return functions as
-- return values. A function that does either of those is called a higher order
-- function. Higher order functions aren't just a part of the Haskell experience,
-- they pretty much are the Haskell experience.
