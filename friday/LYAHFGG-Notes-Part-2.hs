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
