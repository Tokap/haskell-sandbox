---------------- Exercise 4.1.1 (First Excercise Box) -------------------------

-- Write safe versions of multiple existing Haskell functions
-- Completed in safe-definitions.hs

---------------- Exercise 4.1.2 (First Excercise Box) -------------------------

-- Write a function splitWith that acts similarly to words but takes a predicate
-- and a list of any type, then splits its input list on EVERY ELEMENT for
-- which the predicate returns FALSE. So:

-- The predicate given will be a function like odd, even, etc.
-- The way 'words' functions, it removes the white space entirely unless the line
-- is only whitespace. This means for this exercise the predicate should
-- be the only type of element present in the final arrays. This kind of
-- behavior makes sense because it makes the function different from
-- takeWhile or dropWhile - both will actually need to be used in this function.

---------------- Exercise 4.1.3 (First Excercise Box) -------------------------

-- Final result in PrintFirst.hs

---------------- Exercise 4.1.4 (First Excercise Box) -------------------------
-- THURSDAY:
-- OUTLINE:

-- Transpose a text file very similar to the way that
-- built in functions handle combining two lists. Each new list is a new line.

-- going to use: zip for this process
-- First, convert text lines into something useable like in prior exercise
-- Then, take these lists and zip them
-- Finally, place results into output text file


------------ Exercise 4.2.1 & 4.2.2 (Second Excercise Box) ---------------------

-- Rewrite asInt using a fold

---------------- Exercise 4.2.3 (Second Excercise Box) -------------------------
