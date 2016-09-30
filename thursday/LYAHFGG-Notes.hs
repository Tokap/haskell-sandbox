--ARTICLE NATE POSTED REGARDING FUNCTIONAL PROGRAMMING:

-- https://medium.com/@cameronp/functional-programming-is-not-weird-you-just-need-some-new-patterns-7a9bf9dc2f77#.bbi7julun

-- Solve the same kinds of problems over and over. These micropatterns need to
-- become almost like muscle-memory for you, and the only way to achieve that is
-- to do them many, many times.

-- LEARN YOU A HASKELL... :

-- List comprehensions are very similar to set comprehensions. We'll stick to
-- getting the first 10 even numbers for now. The list comprehension we could use
-- is [x*2 | x <- [1..10]]. x is drawn from [1..10] and for every element in
-- [1..10] (which we have bound to x), we get that element, only doubled.
-- Here's that comprehension in action.


---------- COMPREHENSION EXAMPLES WITH PREDICATES --------------------
-- Now let's add a condition (or a predicate) to that comprehension. Predicates
-- go after the binding parts and are separated from them by a comma. Let's say
-- we want only the elements which, doubled, are greater than or equal to 12.
-- (EX1):
ghci> [x*2 | x <- [1..10], x*2 >= 12]
[12,14,16,18,20]

-- Note that weeding out lists by predicates is also called filtering
--------------------------------------------------------------------------------

-- Let's say we want a comprehension that replaces each odd number greater than
-- 10 with "BANG!" and each odd number that's less than 10 with "BOOM!". If a
-- number isn't odd, we throw it out of our list. For convenience, we'll put
-- that comprehension inside a function so we can easily reuse it.
-- (EX2):
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- The above makes complete sense: we tell the list how to behave after the pipe
-- Then, to the left of the pipe we declare how extracted variables who qualify
-- for the pre-set condition (established on the right) will behave if they pass.
--------------------------------------------------------------------------------

-- We can include several predicates. If we wanted all numbers from
-- 10 to 20 that are not 13, 15 or 19, we'd do:
-- (EX3):
ghci> [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
[10,11,12,14,16,17,18,20]
--------------------------------------------------------------------------------

-- If we have two lists, [2,5,10] and [8,10,11] and we want to get the products of
-- all the possible combinations between numbers in those lists, here's what we'd do.
-- (EX4):
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]
[16,20,22,40,50,55,80,100,110]
--------------------------------------------------------------------------------
