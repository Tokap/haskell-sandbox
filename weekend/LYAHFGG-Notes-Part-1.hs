----------- Input and Output -----------

-- WHY FUNCTIONAL PURITY IS GOOD - Best quote ever?
-- In an imperative language, you have no guarantee that a simple function that
-- should just crunch some numbers won't burn down your house, kidnap your dog
-- and scratch your car with a potato while crunching those numbers.

---- WHEN COMPILING:
-- ghc --make helloworld

--ABOUT putStrLn
ghci> :t putStrLn
putStrLn :: String -> IO ()
ghci> :t putStrLn "hello, world"
putStrLn "hello, world" :: IO ()

-- We can read the type of putStrLn like this: putStrLn takes a string and
-- returns an I/O action that has a result type of () (i.e. the empty tuple,
-- also know as unit).

------- What is an I/O action
-- An I/O action is something that, when performed, will carry out an action
-- with a side-effect (that's usually either reading from the input or printing
-- stuff to the screen) and will also contain some kind of return value inside
-- it. Printing a string to the terminal doesn't really have any kind of
-- meaningful return value, so a dummy value of () is used.

-- An I/O action will be performed when we give it a name of main and
-- then run our program.

----------- MULTIPLE ACITONS TOGETHER & DO BLOCKS
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

-- name <- getLine (This can be read as:)
-- perform the I/O action getLine and then bind its result value to name.

-- Once it's fetched that data for you, the only way to open the box and get
-- the data inside it is to use the <- construct.

-- Attributing the I/O content to a variable made it a pure String

-- in a do block, the last action cannot be bound to a name

------- LET BINDINGS
import Data.Char

main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

-- You use let when dealing with the pure aspects & <- to extract out of impure
-- the back arrow will have other implications later

-- Now we're going to make a program that continuously reads a line and prints
-- out the same line with the words reversed. The program's execution will stop
-- when we input a blank line. This is the program:
main = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- USEFUL COMMAND TO AVOID COMPILE;
runhaskell helloworld.hs

-- In an I/O do block, ifs have to have a form of if condition
-- then I/O action else I/O action.

------- HASKELL RETURN
 -- In Haskell (in I/O actions specifically), return makes an I/O action out
 -- of a pure value.

-- While return takes a value and wraps it up in a box, <- takes a box
-- (and performs it) and takes the value out of it, binding it to a name


--------------- USEFUL FUNCTIONS ----------
-- putStr is much like putStrLn in that it takes a string as a parameter and
-- returns an I/O action that will print that string to the terminal, only
-- putStr doesn't jump into a new line after printing out the string while
-- putStrLn does.

-- putChar takes a character and returns an I/O action that will print
-- it out to the terminal.
main = do   putChar 't'
            putChar 'e'
            putChar 'h'
$ runhaskell putchar_test.hs
teh

-- print takes a value of any type that's an instance of Show (meaning that we
-- know how to represent it as a string), calls show with that value to
-- stringify it and then outputs that string to the terminal.
-- Basically, it's just putStrLn . show.

-- getChar is an I/O action that reads a character from the input.
-- Thus, its type signature is getChar :: IO Char
main = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main
        else return ()

$ runhaskell getchar_test.hs
hello sir
hello

-- prints until it hits a space
