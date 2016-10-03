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

-- writeFile
writeFile :: FilePath -> String -> IO ()
-- It takes a path to a file and a string to write to that file and returns an
-- I/O action that will do the writing. If such a file already exists, it will
-- be stomped down to zero length before being written on.
------- EXAMPLE:
import System.IO
import Data.Char

main = do
    contents <- readFile "girlfriend.txt"
    writeFile "girlfriendcaps.txt" (map toUpper contents)

$ runhaskell girlfriendtocaps.hs
$ cat girlfriendcaps.txt
HEY! HEY! YOU! YOU!
I DON'T LIKE YOUR GIRLFRIEND!
NO WAY! NO WAY!
I THINK YOU NEED A NEW ONE!

------- STANDARD APPEND VS WRITE. APPEND WILL ADD, WRITE OVERWRITES
-- appendFile has a type signature that's just like writeFile, only appendFile
-- doesn't truncate the file to zero length if it already exists but it appends
-- stuff to it.

----------- INTERESTNG NOTES CONCERNING LAZY PROCESSING ------------------------
Ooh, one more thing. We talked about how doing contents <- hGetContents handle doesn't cause the whole file to be read at once and stored in-memory. It's I/O lazy, so doing this:

main = do
    withFile "something.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
-- is actually like connecting a pipe from the file to the output. Just like you
-- can think of lists as streams, you can also think of files as streams. This
-- will read one line at a time and print it out to the terminal as it goes
-- along. So you may be asking, how wide is this pipe then? How often will the
-- disk be accessed? Well, for text files, the default buffering is
-- line-buffering usually. That means that the smallest part of the file to be
-- read at once is one line. That's why in this case it actually reads a line,
-- prints it to the output, reads the next line, prints it, etc. For binary
-- files, the default buffering is usually block-buffering. That means that it
-- will read the file chunk by chunk. The chunk size is some size that your
-- operating system thinks is cool.

---------------------- CONTROLLING BUFFERING FLOW ------------------------------
-- You can control how exactly buffering is done by using the hSetBuffering
-- function. It takes a handle and a BufferMode and returns an I/O action that
-- sets the buffering. BufferMode is a simple enumeration data type and the
-- possible values it can hold are: NoBuffering, LineBuffering or BlockBuffering
-- (Maybe Int). The Maybe Int is for how big the chunk should be, in bytes. If
-- it's Nothing, then the operating system determines the chunk size.
-- NoBuffering means that it will be read one character at a time. NoBuffering
-- usually sucks as a buffering mode because it has to access the disk so much.

-- hFlush - we can use hFlush to force that reporting of data that has been
-- read so far. After flushing, the data is available to other programs that are
-- running at the same time.

--------------------------- EDITING A FILE -------------------------------------
import System.IO
import System.Directory
import Data.List

main = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "These are your TO-DO items:"
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"


-- The reason we didn't use getCurrentDirectory to get the current directory and
-- then pass it to openTempFile but instead just passed "." to openTempFile is
-- because . refers to the current directory on unix-like system and Windows

-- removeFile, which, as you can see, takes a path to a file and deletes it.
-- renameFile to rename the temporary file to todo.txt.
---- WARNING:
-- Be careful, removeFile and renameFile (which are both in System.Directory
-- by the way) take file paths as their parameters, not handles.

-------------------- ARGUMENTS ----------------------------
-- The System.Environment module has two cool I/O actions:
-- getArgs, which has a type of getArgs :: IO [String] and is an I/O action that
-- will get the arguments that the program was run with and have as its contained
-- result a list with the arguments.

-- getProgName has a type of getProgName :: IO String and is an I/O action
-- that contains the program name.

---------------- APPLICATION EXAMPLE ------------------------------
-- We'll call it simply todo and it'll be able to do three different things:

-- View tasks
-- Add tasks
-- Delete tasks


import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
            , ("view", view)
            , ("remove", remove)
            ]

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

---------- FUNCTION SUMMARY:
-- To summarize our solution: we made a dispatch association that maps from
-- commands to functions that take some command line arguments and return an
-- I/O action. We see what the command is and based on that we get the
-- appropriate function from the dispatch list. We call that function with the
-- rest of the command line arguments to get back an I/O action that will do the
-- appropriate thing and then just perform that action!

-------------------- RANDOMNESS --------------------------
-- System.Random module

-- random
random :: (RandomGen g, Random a) => g -> (a, g)

ghci> random (mkStdGen 100) :: (Int, StdGen)
(-1352021624,651872571 1655838864)

ghci> random (mkStdGen 949488) :: (Float, StdGen)
(0.8938442,1597344447 1655838864)
ghci> random (mkStdGen 949488) :: (Bool, StdGen)
(False,1485632275 40692)
ghci> random (mkStdGen 949488) :: (Integer, StdGen)
(1691547873,1597344447 1655838864)

---- generating a series of results:
threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in  (firstCoin, secondCoin, thirdCoin)

ghci> threeCoins (mkStdGen 21)
(True,True,True)
ghci> threeCoins (mkStdGen 22)
(True,False,True)
ghci> threeCoins (mkStdGen 943)
(True,False,True)
ghci> threeCoins (mkStdGen 944)
(True,True,True)
--
-- Notice that we didn't have to do random gen :: (Bool, StdGen). That's because
-- we already specified that we want booleans in the type declaration of the
-- function. That's why Haskell can infer that we want a boolean value in
-- this case.

-- randoms that takes a generator and returns an infinite sequence of
-- values based on that generator.
ghci> take 5 $ randoms (mkStdGen 11) :: [Int]
[-1807975507,545074951,-1015194702,-1622477312,-502893664]
ghci> take 5 $ randoms (mkStdGen 11) :: [Bool]
[True,True,True,True,False]
ghci> take 5 $ randoms (mkStdGen 11) :: [Float]
[7.904789e-2,0.62691015,0.26363158,0.12223756,0.38291094]

-- randomR : random value within a range
randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g

ghci> randomR (1,6) (mkStdGen 359353)
(6,1494289578 40692)
ghci> randomR (1,6) (mkStdGen 35935335)
(3,1250031057 40692)

-- There's also randomRs, which produces a stream of random values within our
-- defined ranges. Check this out:

ghci> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]
"ndkxbvmomg"


----- How does Haskell do random? ------
-- System.Random offers the getStdGen I/O action, which has a type of IO StdGen.
-- When your program starts, it asks the system for a good random number
-- generator and stores that in a so called global generator. getStdGen fetches
-- you that global random generator when you bind it to something.
import System.Random

main = do
    gen <- getStdGen
    putStr $ take 20 (randomRs ('a','z') gen)

----- Infinite streams and sampling:
-- One way to get two different strings of length 20 is to set up an infinite
-- stream and then take the first 20 characters and print them out in one line
-- and then take the second set of 20 characters and print them out in the
-- second line.


-- newStdGen action, which splits our current random generator into two
-- generators. It updates the global random generator with one of them and
-- encapsulates the other as its result.
import System.Random

main = do
    gen <- getStdGen
    putStrLn $ take 20 (randomRs ('a','z') gen)
    gen' <- newStdGen
    putStr $ take 20 (randomRs ('a','z') gen')

-- reads - returns an empty list when it fails to read a string.

----------------------------------- Bytestrings --------------------------------
