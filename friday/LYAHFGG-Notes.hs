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

----- Curried functions

-- Functions that take more than one parameter actually take in 1 param and
-- return a function with that param that takes another param and, say, compares
-- The below statements are equivalent:
ghci> max 4 5
5
ghci> (max 4) 5
5

----- Partial Functions (Below is equivalent)
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

-- The type declaration stays the same, because compare 100 returns a function.
-- Compare has a type of (Ord a) => a -> (a -> Ordering) and calling it with 100
-- returns a (Num a, Ord a) => a -> Ordering. The additional class constraint
-- sneaks up there because 100 is also part of the Num typeclass.

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- Taking functions & returning them as params & output
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- Before, we didn't need parentheses because -> is naturally right-associative.
-- However, here, they're mandatory. They indicate that the first parameter is a
-- function that takes something and returns that same thing.

---------- zipWith Example and Explanation
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- Look at the type declaration. The first parameter is a function that takes 
-- two things and produces a third thing. They don't have to be of the same type,
-- but they can. The second and third parameter are lists. The result is also a
-- list. The first has to be a list of a's, because the joining function takes
-- a's as its first argument. The second has to be a list of b's, because the
-- second parameter of the joining function is of type b. The result is a list
-- of c's. If the type declaration of a function says it accepts an a -> b -> c
-- function as a parameter, it will also accept an a -> a -> a function, but not
-- the other way around! Remember that when you're making functions, especially
-- higher order ones, and you're unsure of the type, you can just try omitting
-- the type declaration and then checking what Haskell infers it to be by
-- using :t.
