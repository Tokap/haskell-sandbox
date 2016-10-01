--------------------- MAKING OUR OWN TYPES AND TYPECLASSES ---------------------
-- EXAMPLE OF EXISTING:
data Bool = False | True
-- The parts after the = are value constructors. They specify the different values
-- that this type can have. The | is read as or. So we can read this as: the Bool
-- type can have a value of True or False. Both the type name and the value
-- constructors have to be capital cased.
data Shape = Circle Float Float Float | Rectangle Float Float Float Float

--------- VALUE CONSTRUCTORS ARE ACTUALLY FUNCTIONS THAT ULTIMATELY RETURN
--------- A VALUE OF A DATA TYPE
ghci> :t Circle
Circle :: Float -> Float -> Float -> Shape
ghci> :t Rectangle
Rectangle :: Float -> Float -> Float -> Float -> Shape

----- NEED TO ADD A WAY TP SHOW THE RESULT:
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
--
-- Let's just say that if we add deriving (Show) at the end of a data declaration,
-- Haskell automagically makes that type part of the Show typeclass.
-- So now, we can do this:

---- DEFINING INTERMEDIARY TYPES
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- If we wanted to export the functions and types that we defined here in a
-- module, we could start it off like this:

module Shapes
( Point(..)
, Shape(..)
, surface
, nudge
, baseCircle
, baseRect
) where
-- By doing Shape(..), we exported all the value constructors for Shape,
-- so that means that whoever imports our module can make shapes by using the
-- Rectangle and Circle value constructors. It's the same as writing
-- Shape (Rectangle, Circle).

data Person = Person String String Int Float String String deriving (Show)

ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
ghci> guy
Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
-------------------------------- RECORD SYNTAX ---------------------------------
-------- DEFINING IN SUCH A WAY THAT WE DONT NEED TO HAND WRITE GETTERS:
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

---- A double colon (::) is called the hebrew term Paamayim Nekudotayim
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)
ghci> Car {company="Ford", model="Mustang", year=1967}
Car {company = "Ford", model = "Mustang", year = 1967}

-------------------------------- TYPE PARAMETERS -------------------------------
data Maybe a = Nothing | Just a
-- The a here is the type parameter. And because there's a type parameter
-- involved, we call Maybe a type constructor.

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y
ghci> let stang = Car {company="Ford", model="Mustang", year=1967}
ghci> tellCar stang
"This Ford Mustang was made in 1967"

-- Another example of a parameterized type that we've already met is Map k v from
-- Data.Map. The k is the type of the keys in a map and the v is the type of
-- the values.

-- So don't put type constraints into data declarations even if it seems to make
-- sense, because you'll have to put them into the function type
-- declarations either way.

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq)
-- When we derive the Eq instance for a type and then try to compare two values
-- of that type with == or /=

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)
-- The Show and Read typeclasses are for things that can be converted to or from
-- strings, respectively. Like with Eq, if a type's constructors have fields,
-- their type has to be a part of Show or Read if we want to make our type an
-- instance of them. Let's make our Person data type a part of Show and
-- Read as well.

-- So we can't do read "Just 't'" :: Maybe a,
-- but we can do read "Just 't'" :: Maybe Char.

------------- ORDERING ALGEBRAIC DATA TYPES:
data Bool = False | True deriving (Ord)
-- Because the False value constructor is specified first and the True value
-- constructor is specified after it, we can consider True as greater than False

-- We can easily use algebraic data types to make enumerations and the Enum and
-- Bounded typeclasses help us with that. Consider the following data type:

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

-- The Enum typeclass is for things that have predecessors and successors. We
-- can also make it part of the Bounded typeclass, which is for things that have
-- a lowest possible value and highest possible value.

----- Results:
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

ghci> Wednesday
Wednesday
ghci> show Wednesday
"Wednesday"
ghci> read "Saturday" :: Day
Saturday

ghci> Saturday == Sunday
False
ghci> Saturday == Saturday
True
ghci> Saturday > Friday
True
ghci> Monday `compare` Wednesday
LT

ghci> minBound :: Day
Monday
ghci> maxBound :: Day
Sunday

ghci> succ Monday
Tuesday
ghci> pred Saturday
Friday
ghci> [Thursday .. Sunday]
[Thursday,Friday,Saturday,Sunday]
ghci> [minBound .. maxBound] :: [Day]
[Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]

------- SETTING SYNONYMS
type AssocList k v = [(k,v)]

------- SYNONYM
type IntMap v = Map Int v
type IntMap = Map Int

------- EITHER ------------------
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

--- Uses:
-- when we're interested in how some function failed or why, we usually use the
-- result type of Either a b, where a is some sort of type that can tell us
-- something about the possible failure and b is the type of a successful
-- computation. Hence, errors use the Left value constructor while results
-- use Right.
import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"


lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

ghci> lockerLookup 101 lockers
Right "JAH3I"
ghci> lockerLookup 100 lockers
Left "Locker 100 is already taken!"
ghci> lockerLookup 102 lockers
Left "Locker number 102 doesn't exist!"
ghci> lockerLookup 110 lockers
Left "Locker 110 is already taken!"
ghci> lockerLookup 105 lockers
Right "QOTSA"

------------------------ BINARY SEARCH TREES -----------------------------------
-- Now, we're going to implement a binary search tree. If you're not familiar with
-- binary search trees from languages like C, here's what they are: an element
-- points to two elements, one on its left and one on its right. The element to
-- the left is smaller, the element to the right is bigger. Each of those
-- elements can also point to two elements (or one, or none). In effect, each
-- element has up to two sub-trees. And a cool thing about binary search trees
-- is that we know that all the elements at the left sub-tree of, say, 5 are
-- going to be smaller than 5. Elements in its right sub-tree are going to be
-- bigger. So if we need to find if 8 is in our tree, we'd start at 5 and
-- then because 8 is greater than 5, we'd go right. We're now at 7 and
-- because 8 is greater than 7, we go right again. And we've found our element
-- in three hops! Now if this were a normal list (or a tree, but really unbalanced)
-- , it would take us seven hops instead of three to see if 8 is in there.

-- Sets and maps from Data.Set and Data.Map are implemented using trees,
-- only instead of normal binary search trees, they use balanced binary search
-- trees, which are always balanced
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right
