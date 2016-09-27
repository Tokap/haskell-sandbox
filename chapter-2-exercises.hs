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

-- This function clearly cannot take additional computations, it can't evaluate truthiness,
-- it can't take in as an argument anything other than a list (a String counts as a list)
-- it can't talk to outside networks

---------------- Exercise 2.2.2 (Second Excercise Box) -------------------------

---------------- Exercise 2.2.3 (Second Excercise Box) -------------------------
