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
