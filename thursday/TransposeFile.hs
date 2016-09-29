-- import System.Environment (getArgs)
--
-- interactWith function inputFile outputFile = do
--   input <- readFile inputFile
--   writeFile outputFile (function input)
--
-- main = mainWith myFunction
--   where mainWith function = do
--           args <- getArgs
--           case args of
--             [input,output] -> interactWith function input output
--             _ -> putStrLn "error: exactly two arguments needed"
--         myFunction = id




import System.Environment (getArgs)

printList list = mapM_ putStrLn list

-- I feel as if the series of variable assignments below could be condensed
-- into a better function chain. Attempt to refactor after Chapter completion.
readLines fileName = do
  parsedFile <- readFile fileName
  let fileLines = lines parsedFile
  let headOnly = fmap head fileLines
  let lineWords = fmap words fileLines --List of Lists broken by line
  printList lineWords

main = do
  args <- getArgs
  case args of
    [input] -> readLines input
