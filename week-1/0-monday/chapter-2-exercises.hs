---------------- Exercise 2.1.1 (First Excercise Box) -------------------------
:type False
-- False :: Bool
:type (["foo", "bar"], 'a')
-- (["foo", "bar"], 'a') :: ([[Char]], Char)
:type [(True, []), (False, [['a']])]
-- [(True, []), (False, [['a']])] :: [(Bool, [[Char]])]

---------------- Exercise 2.2.1 (Second Excercise Box) -------------------------
last :: [a] -> a
-- This function is able to return the final value of any valid list.
-- The list can be of any type - Boolean, String, Integer, Int, etc.
-- The list MUST be at least 1 value long, otherwise it will return an exception:
-- *** Exception: Prelude.last: empty list

-- This function clearly cannot take more than 1 argument, it can't evaluate truthiness,
-- it can't take in as an argument anything other than a list (a String counts as a list),
-- it cannot return something that is of a different type than the contents of the original list.

---------------- Exercise 2.2.2 (Second Excercise Box) -------------------------

-- Work shown in last-but-one.hs file.

---------------- Exercise 2.2.3 (Second Excercise Box) -------------------------

-- Based on the way I wrote my function, I receive the following exception:
-- *** Exception: Prelude.!!: negative index
-- The above applies for both an empty list and one with less than 2 values.
