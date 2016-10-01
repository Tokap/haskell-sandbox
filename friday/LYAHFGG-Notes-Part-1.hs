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

---------- map and versatility
ghci> map (++ "!") ["BIFF", "BANG", "POW"]
["BIFF!","BANG!","POW!"]
ghci> map (replicate 3) [3..6]
[[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
ghci> map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]
[1,3,6,2,2]
ghci> filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
"uagameasadifeent"
ghci> filter (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same"
"GAYBALLS"

------- Quicksort using filters:
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted

-- Let's find the largest number under 100,000 that's divisible by 3829. To do
-- that, we'll just filter a set of possibilities in which we know the
-- solution lies.
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

--------- takeWhile function
-- takes a predicate and a list and then goes from the beginning of the list
-- and returns its elements while the predicate holds true.

-- If we wanted to get the first word of the string "elephants know how to
-- party", we could do takeWhile (/=' ') "elephants know how to party"
-- and it would return "elephants".

------------- LAMBDAS ------------------
-- Lambdas are basically anonymous functions that are used because we need
-- some functions only once.

 ------ Style:
-- To make a lambda, we write a \ (because it kind of looks like the greek
-- letter lambda if you squint hard enough) and then we write the parameters,
-- separated by spaces. After that comes a -> and then the function body.

numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))
-- Lambdas are expressions, that's why we can just pass them like that.
-- The expression (\xs -> length xs > 15) returns a function that tells us
-- whether the length of the list passed to it is greater than 15.

---- Dont use lambdas where you dont need to (below is equivalent):
map (+3) [1,6,3,2]
map (\x -> x + 3) [1,6,3,2]

---- With pattern matching:
-- If a pattern matching fails in a lambda, a runtime error occurs, so be
-- careful when pattern matching in lambdas!

-- The most common use case with flip is calling it with just the function
-- parameter and then passing the resulting function on to a map or a filter.
-- So use lambdas in this way when you want to make it explicit that your
-- function is mainly meant to be partially applied and passed on to a function
-- as a parameter.

------------- FOLDS ------------------

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0
-- The lambda function (\acc x -> acc + x) is the same as (+)
-- We can omit the xs as the parameter because calling foldl (+) 0
-- will return a function that takes a list.

-- Generally, if you have a function like foo a = bar b a, you can
-- rewrite it as foo = bar b, because of currying.

-- The type of the accumulator value and the end result is always the same when
-- dealing with folds. Remember that if you ever don't know what to use as a
-- starting value, it'll give you some idea.

-- The accumulator value (and hence, the result) of a fold can be of
-- any type. It can be a number, a boolean or even a new list.

-- Folds can be used to implement any function where you traverse a list once,
-- element by element, and then return something based on that. Whenever you
-- want to traverse a list to return something, chances are you want a fold.
-- That's why folds are, along with maps and filters, one of the most useful
-- types of functions in functional programming.


------------ foldl1 and foldr1
-- you don't need to provide them with an explicit starting value
-- They assume the first (or last) element of the list to be the starting value
sum = foldl1 (+)


---------- fold examples
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)


-------------- scanl and scanr
ghci> scanl (+) 0 [3,5,2,1]
[0,3,8,10,11]

ghci> scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
[3,4,5,5,7,9,9,9]

-- When using a scanl, the final result will be in the last element of
-- the resulting list while a scanr will place the result in the head.

------------------ FUNCTION APPLICATION WITH $ ---------------------
($) :: (a -> b) -> a -> b
f $ x = f x

-- When a $ is encountered, the expression on its right is applied as
-- the parameter to the function on its left.

-- If we want get the square root of 3 + 4 + 9, we'd have to write:
sqrt (3 + 4 + 9)
-- or if we use $ we can write it as
sqrt $ 3 + 4 + 9

------------------------- FUNCTION COMPOSITION ---------------------------------
-- We do function composition with the . function, which is defined like so:
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

------ The below are equivalent
ghci> map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
[-5,-3,-6,-7,-3,-2,-19,-24]

ghci> map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
[-5,-3,-6,-7,-3,-2,-19,-24]

-- Function composition is right-associative, so we can compose many
-- functions at a time.

ghci> map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
[-14,-15,-27]

ghci> map (negate . sum . tail) [[1..5],[3..6],[1..7]]
[-14,-15,-27]

-- The period goes : This, then this, then this
-- The $ goes : x $ y -> all of y, then apply x to it.

------------------ POINT FREE FUNCTION STYLE -------------------------------
-- The xs is exposed on both right sides. Because of currying, we can omit the xs
-- on both sides, because calling foldl (+) 0 creates a function that takes a
-- list. Writing the function as sum' = foldl (+) 0 is called writing it in point
-- free style. How would we write this in point free style?

-- 3 Equivalent statements with different styles:

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum :: Integer
oddSquareSum =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit

------------------------- IMPORTING MODULES ------------------------------------
-- LIST OF STANDARD LIBRARY:
-- https://downloads.haskell.org/~ghc/latest/docs/html/libraries/
-- https://www.haskell.org/hoogle/ (SUPPORT RESOURCE)

import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
-- nub is a function defined in Data.List that takes a list and
-- weeds out duplicate elements

ghci> :m + Data.List
ghci> :m + Data.List Data.Map Data.Set

-- Only certain functions
import Data.List (nub, sort)

-- All EXCEPT:
import Data.List hiding (nub)

-- QUALIFIED IMPORTS:
import qualified Data.Map
-- we have to do Data.Map.filter, whereas just filter still
-- refers to the normal filter

import qualified Data.Map as M
-- Now, to reference Data.Map's filter function, we just use M.filter.

-------------- DATA.LIST LIBRARY -------------------------
-- Intersperse takes an element and a list and then puts that element
-- in between each pair of elements in the list.
ghci> intersperse '.' "MONKEY"
"M.O.N.K.E.Y"
ghci> intersperse 0 [1,2,3,4,5,6]
[1,0,2,0,3,0,4,0,5,0,6]

-- intercalate takes a list of lists and a list. It then inserts that list in
-- between all those lists and then flattens the result.
ghci> intercalate " " ["hey","there","guys"]
"hey there guys"
ghci> intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]
[1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]

-- transpose transposes a list of lists. If you look at a list of lists as a
-- 2D matrix, the columns become the rows and vice versa.
ghci> transpose [[1,2,3],[4,5,6],[7,8,9]]
[[1,4,7],[2,5,8],[3,6,9]]
ghci> transpose ["hey","there","guys"]
["htg","ehu","yey","rs","e"]

-- concat flattens a list of lists into just a list of elements.
ghci> concat ["foo","bar","car"]
"foobarcar"
ghci> concat [[3,4,5],[2,3,4],[2,1,1]]
[3,4,5,2,3,4,2,1,1]

-- concatMap is the same as first mapping a function to a list and
-- then concatenating the list with concat.
ghci> concatMap (replicate 4) [1..3]
[1,1,1,1,2,2,2,2,3,3,3,3]

-- and takes a list of boolean values and returns True only if all the values
-- in the list are True.
ghci> and $ map (>4) [5,6,7,8]
True
ghci> and $ map (==4) [4,4,4,3,4]
False

-- or is like and, only it returns True if any of the boolean values in
-- a list is True.
ghci> or $ map (==4) [2,3,4,5,6,1]
True
ghci> or $ map (>4) [1,2,3]
False

-- any and all take a predicate and then check if any or all the elements in
-- a list satisfy the predicate, respectively.
ghci> any (==4) [2,3,5,6,1,4]
True
ghci> all (>4) [6,9,10]
True
ghci> all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
False
ghci> any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"
True

-- iterate takes a function and a starting value. It applies the function to the
-- starting value, then it applies that function to the result, then it applies
-- the function to that result again, etc. It returns all the results in the
-- form of an infinite list.
ghci> take 10 $ iterate (*2) 1
[1,2,4,8,16,32,64,128,256,512]
ghci> take 3 $ iterate (++ "haha") "haha"
["haha","hahahaha","hahahahahaha"]

-- splitAt takes a number and a list. It then splits the list at that many
-- elements, returning the resulting two lists in a tuple
ghci> splitAt 3 "heyman"
("hey","man")
ghci> splitAt 100 "heyman"
("heyman","")
ghci> splitAt (-3) "heyman"
("","heyman")
ghci> let (a,b) = splitAt 3 "foobar" in b ++ a
"barfoo"

-- takeWhile is a really useful little function. It takes elements from a list
-- while the predicate holds and then when an element is encountered that doesn't
-- satisfy the predicate, it's cut off.
ghci> takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]
[6,5,4]
ghci> takeWhile (/=' ') "This is a sentence"
"This"
ghci> sum $ takeWhile (<10000) $ map (^3) [1..]
53361

-- dropWhile is similar, only it drops all the elements while the predicate is
-- true. Once predicate equates to False, it returns the rest of the list.
ghci> dropWhile (/=' ') "This is a sentence"
" is a sentence"
ghci> dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]
[3,4,5,4,3,2,1]

-- span is kind of like takeWhile, only it returns a pair of lists. The first
-- list contains everything the resulting list from takeWhile would contain if
-- it were called with the same predicate and the same list. The second list
-- contains the part of the list that would have been dropped.

-- Whereas span spans the list while the predicate is true, break breaks it
-- when the predicate is first true.
ghci> break (==4) [1,2,3,4,5,6,7]
([1,2,3],[4,5,6,7])
ghci> span (/=4) [1,2,3,4,5,6,7]
([1,2,3],[4,5,6,7])

-- sort simply sorts a list

-- group takes a list and groups adjacent elements into sublists if they are equal.
ghci> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]

-- inits and tails are like init and tail, only they recursively apply that to a
-- list until there's nothing left.
ghci> inits "w00t"
["","w","w0","w00","w00t"]
ghci> tails "w00t"
["w00t","00t","0t","t",""]
ghci> let w = "w00t" in zip (inits w) (tails w)
[("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")]

---- More useful functions:
-- isInfixOf
-- isPrefixOf
-- isSuffixOf
-- elem
-- notElem
-- zip3 (up to 7)
-- zipWith3 (up to 7)

-- partition takes a list and a predicate and returns a pair of lists. The
-- first list in the result contains all the elements that satisfy the predicate,
-- the second contains all the ones that don't.
ghci> partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"
("BOBMORGAN","sidneyeddy")
ghci> partition (>3) [1,3,5,6,3,2,1,0,3,7]
([5,6,7],[1,3,3,2,1,0,3])

---- Partition vs Span or Break
-- While span and break are done once they encounter the first element that
-- doesn't and does satisfy the predicate, partition goes through the whole
-- list and splits it up according to the predicate.

-- find takes a list and a predicate and returns the first element
-- that satisfies the predicate - returning that elem wrapped in a Maybe value.
-- a Maybe value can either be Just something or Nothing.
ghci> find (>4) [1,2,3,4,5,6]
Just 5
ghci> find (>9) [1,2,3,4,5,6]
Nothing
ghci> :t find
find :: (a -> Bool) -> [a] -> Maybe a
-- a value of the type Maybe can contain either no elements or one element

----- UNSAFE IF IT RETURNS AN EMPTY ARRAY FOR HEAD TO TARGET:
head (dropWhile (\(val,y,m,d) -> val < 1000) stock)

----- SAFE BECAUSE OF THE POWER OF MAYBE:
 find (\(val,y,m,d) -> val > 1000) stock

-- elemIndex is kind of like elem, only it doesn't return a boolean value.
-- It maybe returns the index of the element we're looking for. If that element
-- isn't in our list, it returns a Nothing.
ghci> :t elemIndex
elemIndex :: (Eq a) => a -> [a] -> Maybe Int
ghci> 4 `elemIndex` [1,2,3,4,5,6]
Just 3
ghci> 10 `elemIndex` [1,2,3,4,5,6]
Nothing

-- elemIndices is like elemIndex, only it returns a list of indices, in case
-- the element we're looking for crops up in our list several times. Because
-- we're using a list to represent the indices, we don't need a Maybe type,
-- because failure can be represented as the empty list, which is very much
-- synonymous to Nothing.
ghci> ' ' `elemIndices` "Where are the spaces?"
[5,9,13]

-- findIndex is like find, but it maybe returns the index of the first element
-- that satisfies the predicate.
-- findIndices returns the indices of all elements
-- that satisfy the predicate in the form of a list.
ghci> findIndex (==4) [5,3,2,1,6,4]
Just 5
ghci> findIndex (==7) [5,3,2,1,6,4]
Nothing
ghci> findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"
[0,6,10,14]

-- lines is a useful function when dealing with files or input from somewhere.
-- It takes a string and returns every line of that string in a separate list
ghci> lines "first line\nsecond line\nthird line"
["first line","second line","third line"]

-- unlines is the inverse function of lines. It takes a list of strings and
-- joins them together using a '\n'.

ghci> unlines ["first line", "second line", "third line"]
"first line\nsecond line\nthird line\n"

-- words and unwords are for splitting a line of text into words or joining
-- a list of words into a text.
ghci> words "hey these are the words in this sentence"
["hey","these","are","the","words","in","this","sentence"]
ghci> words "hey these           are    the words in this\nsentence"
["hey","these","are","the","words","in","this","sentence"]
ghci> unwords ["hey","there","mate"]
"hey there mate"

-- nub removed duplicates
ghci> nub [1,2,3,4,3,2,1,2,3,4,3,2,1]
[1,2,3,4]
ghci> nub "Lots of words and stuff"
"Lots fwrdanu"

-- delete takes an element and a list and deletes the first occurence of
-- that element in the list.
ghci> delete 'h' "hey there ghang!"
"ey there ghang!"
ghci> delete 'h' . delete 'h' $ "hey there ghang!"
"ey tere ghang!"
ghci> delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!"
"ey tere gang!"

-- \\ is the list difference function. It acts like a set difference, basically.
-- For every element in the right-hand list, it removes a matching element in
-- the left one.
-- Doing [1..10] \\ [2,5,9] is like doing delete 2 . delete 5 . delete 9 $ [1..10].
ghci> [1..10] \\ [2,5,9]
[1,3,4,6,7,8,10]
ghci> "Im a big baby" \\ "big"
"Im a  baby"

-- union also acts like a function on sets. It returns the union of two lists.
-- It pretty much goes over every element in the second list and appends it to
-- the first one if it isn't already in yet. Watch out though, duplicates are
-- removed from the second list!
ghci> "hey man" `union` "man what's up"
"hey manwt'sup"
ghci> [1..7] `union` [5..10]
[1,2,3,4,5,6,7,8,9,10]

-- intersect works like set intersection. It returns only the elements that
 -- are found in both lists.
 ghci> [1..7] `intersect` [5..10]
[5,6,7]

-- insert takes an element and a list of elements that can be sorted and inserts
-- it into the last position where it's still less than or equal to the next
-- element
ghci> insert 4 [3,5,1,2,8,2]
[3,4,5,1,2,8,2]
ghci> insert 4 [1,3,4,4,1]
[1,3,4,4,4,1]

-------- FOR MORE GENERIC VERSION OF PRACTICAL FUNCTIONS -----------------------
-- genericLength
-- genericTake
-- genericDrop
-- genericSplitAt
-- genericIndex
-- genericReplicate
-- length :: [a] -> Int (vs) genericLength :: (Num a) => [b] -> a
--
-- nubBy
-- deleteBy
-- unionBy
-- intersectBy
-- groupBy

-- group is the same as groupBy (==).

-- sortBy
-- insertBy
-- maximumBy
-- minimumBy

-- sort is the equivalent of sortBy compare

------------ Data.Function - using 'on' ----------
-- You can read it out loud: Group this by equality on whether the elements
-- are greater than zero.
ghci> groupBy ((==) `on` (> 0)) values
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]

ghci> let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]
ghci> sortBy (compare `on` length) xs
[[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]

-- compare `on` length is the equivalent of \x y -> length x `compare` length y

-------------- DATA.CHAR LIBRARY -------------------------
-- isControl: checks whether a character is a control character.
-- isSpace: checks whether a character is a white-space characters.
-- That includes spaces, tab characters, newlines, etc.
-- isLower: checks whether a character is lower-cased.
-- isUpper: checks whether a character is upper-cased.
-- isAlpha: checks whether a character is a letter.
-- isAlphaNum: checks whether a character is a letter or a number.
-- isPrint: checks whether a character is printable. Control characters,
-- for instance, are not printable.
-- isDigit: checks whether a character is a digit.
-- isOctDigit: checks whether a character is an octal digit.
-- isHexDigit: checks whether a character is a hex digit.
-- isLetter: checks whether a character is a letter.
-- isMark: checks for Unicode mark characters. Those are characters that combine
-- with preceding letters to form latters with accents. Use this if you are French.
-- isNumber: checks whether a character is numeric.
-- isPunctuation: checks whether a character is punctuation.
-- isSymbol: checks whether a character is a fancy mathematical or currency symbol.
-- isSeparator: checks for Unicode spaces and separators.
-- isAscii: checks whether a character falls into the first 128 characters of the
-- Unicode character set.
-- isLatin1: checks whether a character falls into the first 256 characters of
-- Unicode.
-- isAsciiUpper: checks whether a character is ASCII and upper-case.
-- isAsciiLower: checks whether a character is ASCII and lower-case.

ghci> all isAlphaNum "bobby283"
True
ghci> all isAlphaNum "eddy the fish!"
False

-------------- generalCategory -------------------
ghci> generalCategory ' '
Space
ghci> generalCategory 'A'
UppercaseLetter
ghci> generalCategory 'a'
LowercaseLetter
ghci> generalCategory '.'
OtherPunctuation
ghci> generalCategory '9'
DecimalNumber
ghci> map generalCategory " \t\nA9?|"
[Space,Control,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol]

-- toUpper: converts a character to upper-case. Spaces, numbers, and the like
-- remain unchanged.
-- toLower: converts a character to lower-case.
-- toTitle: converts a character to title-case. For most characters, title-case
-- is the same as upper-case.
-- digitToInt: converts a character to an Int. To succeed, the character must
-- be in the ranges '0'..'9', 'a'..'f' or 'A'..'F'.
ghci> map digitToInt "34538"
[3,4,5,3,8]
ghci> map digitToInt "FF85AB"
[15,15,8,5,10,11]

-- intToDigit is the inverse function of digitToInt. It takes an Int in the
-- range of 0..15 and converts it to a lower-case character.
ghci> intToDigit 15
'f'
ghci> intToDigit 5
'5'
-- The ord and chr functions convert characters to their corresponding
-- numbers and vice versa:

ghci> ord 'a'
97
ghci> chr 97
'a'
ghci> map ord "abcdefgh"
[97,98,99,100,101,102,103,104]

-------------- DATA.MAP LIBRARY -------------------------

-- Association lists (also called dictionaries) are lists that are used to
-- store key-value pairs where ordering doesn't matter.
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]

----- Using Maybe
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) = if key == k
                            then Just v
                            else findKey key xs
-- lookup: If we want to find the corresponding value to a key, we have to
-- traverse all the elements of the list until we find it.

---------- WORKING WITH MAPS:
-- import qualified Data.Map as Map

-- The fromList function takes an association list (in the form of a list) and
-- returns a map with the same associations. If there are duplicate keys in the
-- original association list, the duplicates are just discarded. This is the
-- type signature of fromList
ghci> Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]
fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]
ghci> Map.fromList [(1,2),(3,4),(3,2),(5,5)]
fromList [(1,2),(3,2),(5,5)]

-- empty: represents an empty map. Takes no arguments - just returns an empty map.
-- insert: takes a key, a value and a map and returns a new map that's just
-- like the old one, only with the key and value inserted.
ghci> Map.empty
fromList []
ghci> Map.insert 3 100 Map.empty
fromList [(3,100)]
ghci> Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty))
fromList [(3,100),(4,200),(5,600)]
ghci> Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty
fromList [(3,100),(4,200),(5,600)]

-- null checks if a map is empty.
ghci> Map.null Map.empty
True
ghci> Map.null $ Map.fromList [(2,3),(5,5)]
False

-- size reports the size of a map.
ghci> Map.size Map.empty
0
ghci> Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]
5

-- singleton takes a key and a value and creates a map that has exactly one mapping.
ghci> Map.singleton 3 9
fromList [(3,9)]
ghci> Map.insert 5 9 $ Map.singleton 3 9
fromList [(3,9),(5,9)]

-- lookup works like the Data.List lookup, only it operates on maps. It returns
-- Just something if it finds something for the key and Nothing if it doesn't.
-- member is a predicate takes a key and a map and reports whether the key is
-- in the map or not.
ghci> Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]
True
ghci> Map.member 3 $ Map.fromList [(2,5),(4,5)]
False

-- map and filter work much like their list equivalents.
ghci> Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]
fromList [(1,100),(2,400),(3,900)]
ghci> Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]
fromList [(2,'A'),(4,'B')]

-- toList is the inverse of fromList.
ghci> Map.toList . Map.insert 9 2 $ Map.singleton 4 3
[(4,3),(9,2)]

-- keys and elems return lists of keys and values respectively. keys is the
-- equivalent of map fst . Map.toList and elems is the equivalent of
-- map snd . Map.toList.
--
-- fromListWith is a cool little function. It acts like fromList, only it
-- doesn't discard duplicate keys but it uses a function supplied to it to
-- decide what to do with them.

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs
ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook
"827-9162, 943-2929, 493-2928"
ghci> Map.lookup "wendy" $ phoneBookToMap phoneBook
"939-8282"
ghci> Map.lookup "betty" $ phoneBookToMap phoneBook
"342-2492, 555-2938"

-- ALT:
-- phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
-- phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs
-- ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook
-- ["827-9162","943-2929","493-2928"]

-- insertWith is to insert what fromListWith is to fromList. It inserts a
-- key-value pair into a map, but if that map already contains the key, it uses
-- the function passed to it to determine what to do.
ghci> Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]
fromList [(3,104),(5,103),(6,339)]

----------------------------------- DATA.SET -----------------------------------
import qualified Data.Set as Set
text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

-- fromList
ghci> let set1 = Set.fromList text1
ghci> let set2 = Set.fromList text2
ghci> set1
fromList " .?AIRadefhijlmnorstuy"
ghci> set2
fromList " !Tabcdefghilmnorstuvwy"

-- intersection
ghci> Set.intersection set1 set2
fromList " adefhilmnorstuy"

-- difference
ghci> Set.difference set1 set2
fromList ".?AIRj"
ghci> Set.difference set2 set1
fromList "!Tbcgvw"

-- Or we can see all the unique letters used in both sentences by using union.
ghci> Set.union set1 set2
fromList " !.?AIRTabcdefghijlmnorstuvwy"

-- all these work like you'd expect them to.
-- null
-- size
-- member
-- empty
-- singleton
-- insert
-- delete
ghci> Set.null Set.empty
True
ghci> Set.null $ Set.fromList [3,4,5,5,4,3]
False
ghci> Set.size $ Set.fromList [3,4,5,3,4,5]
3
ghci> Set.singleton 9
fromList [9]
ghci> Set.insert 4 $ Set.fromList [9,3,8,1]
fromList [1,3,4,8,9]
ghci> Set.insert 8 $ Set.fromList [5..10]
fromList [5,6,7,8,9,10]
ghci> Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5]
fromList [3,5]

-- We can also map over sets and filter them.
ghci> Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]
fromList [3,5,7]
ghci> Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4]
fromList [3,4,5,6,7,8]

-- Sets are often used to weed a list of duplicates from a list by first making it
-- into a set with fromList and then converting it back to a list with toList.

ghci> let setNub xs = Set.toList $ Set.fromList xs
ghci> setNub "HEY WHATS CRACKALACKIN"
" ACEHIKLNRSTWY"
ghci> nub "HEY WHATS CRACKALACKIN"
"HEY WATSCRKLIN"
--
-- setNub is generally faster than nub on big lists but as you can see, nub
-- preserves the ordering of the list's elements, while setNub does not.

--------------------------------- DIY MODULES ----------------------------------
module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi * (radius ^ 2)

cubeVolume :: Float -> Float
cubeVolume side = cuboidVolume side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = rectangleArea a b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b

import Geometry
-- Geometry.hs has to be in the same folder that the program that's
-- importing it is in, though.
