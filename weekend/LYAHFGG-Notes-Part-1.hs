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


------------------ THE WHEN FUNCTION ----------------
-- The when function is found in Control.Monad (to get access to it, do
-- import Control.Monad). It's interesting because in a do block it looks
-- like a control flow statement, but it's actually a normal function. It
-- takes a boolean value and an I/O action if that boolean value is True,
-- it returns the same I/O action that we supplied to it. However, if it's
-- False, it returns the return (), action, so an I/O action that doesn't
-- do anything.

import Control.Monad

main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main

-- it's useful for encapsulating the if something then do some
-- I/O action else return () pattern.

------------------ SEQUENCE ----------------
-- Takes a list of I/O actions and returns an I/O action that will perform
-- those actions one after the other. The result contained in that I/O action
-- will be a list of the results of all the I/O actions that were performed.
-- Its type signature is -
sequence :: [IO a] -> IO [a]. Doing this:

main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]
-- Is exactly the same as doing this:

main = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
-- So sequence [getLine, getLine, getLine] makes an I/O action that
-- will perform getLine three times. A common pattern with sequence is when
-- we map functions like print or putStrLn over lists.
ghci> sequence (map print [1,2,3,4,5])
1
2
3
4
5
[(),(),(),(),()]

---------------------------- mapM & mapM_ -------------------------------------
-- Because mapping a function that returns an I/O action over a list and then
-- sequencing it is so common, the utility functions mapM and mapM_

-- mapM takes a function and a list, maps the function over the list and then
-- sequences it. mapM_ does the same, only it throws away the result later.
-- We usually use mapM_ when we don't care what result our sequenced I/O a
-- ctions have.

ghci> mapM print [1,2,3]
1
2
3
[(),(),()]
ghci> mapM_ print [1,2,3]
1
2
3

---------------------------- forever -------------------------------------
-- forever takes an I/O action and returns an I/O action that just repeats the
-- I/O action it got forever. It's located in Control.Monad. This little program
-- will indefinitely ask the user for some input and spit it back to him,
-- CAPSLOCKED:

import Control.Monad
import Data.Char

main = forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l

-------------------------- forM & Lambdas -----------------------------------
-- You can think of forM as meaning: make an I/O action for every element
-- in this list.
import Control.Monad

main = do
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
        color <- getLine
        return color)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors
-- The (\a -> do ... ) is a function that takes a number and returns an I/O
-- action. We have to surround it with parentheses, otherwise the lambda thinks
-- the last two I/O actions belong to it. Notice that we do return color in the
-- inside do block. We do that so that the I/O action which the do block defines
-- has the result of our color contained within it. We actually didn't have to
-- do that, because getLine already has that contained within it. Doing color
-- <- getLine and then return color is just unpacking the result from getLine
-- and then repackaging it again, so it's the same as just doing getLine. The
-- forM (called with its two parameters) produces an I/O action, whose result we
-- bind to colors. colors is just a normal list that holds strings. At the end,
-- we print out all those colors by doing mapM putStrLn colors.


-- We could have actually done that without forM, only with forM it's more
-- readable. Normally we write forM when we want to map and sequence some
-- actions that we define there on the spot using do notation. In the same
-- vein, we could have replaced the last line with forM colors putStrLn.

------------------ getContents ------------------------------------
-- getContents is an I/O action that reads everything from the standard input
-- until it encounters an end-of-file character.

-- getContents is really useful when we're piping the output from one program
-- into the input of our program.

------ Using pipes:
$ ghc --make capslocker
[1 of 1] Compiling Main             ( capslocker.hs, capslocker.o )
Linking capslocker ...
$ cat haiku.txt
I'm a lil' teapot
What's with that airplane food, huh?
It's so small, tasteless
$ cat haiku.txt | ./capslocker
I'M A LIL' TEAPOT
WHAT'S WITH THAT AIRPLANE FOOD, HUH?
IT'S SO SMALL, TASTELESS
capslocker <stdin>: hGetLine: end of file

-- to escape a forever loop in the terminal - pressing Ctrl-D

-- This pattern of getting some string from the input, transforming it with a
-- function and then outputting that is so common that there exists a function
-- which makes that even easier, called interact

--------------------- Interact --------------------------
-- interact takes a function of type String -> String as a parameter and returns
-- an I/O action that will take some input, run that function on it and then
-- print out the function's result.

---- ORIGINAL:
main = do
    contents <- getContents
    putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in  result

---- WITH INTERACT:
main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in  result

---- SHORTENED WITH FUNCTION CHAINING:
main = interact $ unlines . filter ((<10) . length) . lines

-- interact can be used to make programs that are piped some contents into them
-- and then dump some result out or it can be used to make programs that appear
-- to take a line of input from the user, give back some result based on that
-- line and then take another line and so on. There isn't actually a real
-- distinction between the two, it just depends on how the user is supposed
-- to use them.

--------------------------------------------------------------------------------
----------- EXAMPLE OF FUNCTIONAL THOUGHT PROCESS FOR AN I/O ACTION ------------
--------------------------------------------------------------------------------
-- Let's make a program that continuously reads a line and then tells us if the
-- line is a palindrome or not. We could just use getLine to read a line, tell
-- the user if it's a palindrome and then run main all over again. But it's
-- simpler if we use interact. When using interact, think about what you need to
-- do to transform some input into the desired output. In our case, we have to
-- replace each line of the input with either "palindrome" or "not a palindrome".
-- So we have to write a function that transforms something like
-- "elephant\nABCBA\nwhatever" into
-- "not a palindrome\npalindrome\nnot a palindrome".

respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))
    where   isPalindrome xs = xs == reverse xs
-- Point free version:
respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
    where   isPalindrome xs = xs == reverse xs

main = interact respondPalindromes
-- We can also use this program by just piping a file into it.
-- Let's say we have this file:

dogaroo
radar
rotor
madam
-- and we save it as words.txt. This is what we get by piping it into our program:

$ cat words.txt | runhaskell palindromes.hs
not a palindrome
palindrome
palindrome
palindrome

----------------------- WORKING WITH FILES -------------------------------------
import System.IO

main = do
    handle <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
-- OUTPUT:
$ runhaskell girlfriend.hs
Hey! Hey! You! You!
I don't like your girlfriend!
No way! No way!
I think you need a new one!



-- openFile:
openFile :: FilePath -> IOMode -> IO Handle
-- openFile takes a file path and an IOMode and returns an I/O action that will
-- open a file and have the file's associated handle encapsulated as its result.

-- FilePath is just a type synonym for String, simply defined as:
type FilePath = String

-- IOMode is a type that's defined like this:
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

------------------------ handle & hGetContents ---------------------------
-- A value of type Handle represents where our file is.
-- hGetContents - It takes a Handle, so it knows which file to get the contents
-- from and returns an IO String — an I/O action that holds as its result the
-- contents of the file.
-- getContents will automatically read from the standard input (that is from the
-- terminal), whereas hGetContents takes a file handle which tells it which file
-- to read from. In all other respects, they work the same.

---------------------- Close the door when you're done -----------------------
-- hClose, which takes a handle and returns an I/O action that closes the file.
-- You have to close the file yourself after opening it with openFile!

----- ALTERNATIVELY:
 -- withFile function, which has a type signature of
 withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
 -- It takes a path to a file, an IOMode and then it takes a function that takes
 -- a handle and returns some I/O action. What it returns is an I/O action that
 -- will open that file, do something we want with the file and then close it.
 import System.IO

 main = do
     withFile "girlfriend.txt" ReadMode (\handle -> do
         contents <- hGetContents handle
         putStr contents)

------ Hanlder functions:
-- hGetLine, hPutStr, hPutStrLn, hGetChar, etc.
-- They work just like their counterparts without the h, only they take a
-- handle as a parameter and operate on that specific file instead of operating
-- on standard input or standard output.
readFile :: FilePath -> IO String

-- readFile takes a path to a file and returns an I/O action that will read that
-- file (lazily, of course) and bind its contents to something as a string.
-- READFILE IS GENERALLY EASIER THAN openFile, bind to handle, hGetContents.

import System.IO

main = do
    contents <- readFile "girlfriend.txt"
    putStr contents
-- Because we don't get a handle with which to identify our file, we can't close 
-- it manually, so Haskell does that for us when we use readFile.
