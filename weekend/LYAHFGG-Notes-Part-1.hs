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
