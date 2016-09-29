import System.Environment (getArgs)

printFirst function inputFile = do
  input <- readFile inputFile
  printList (lines input)

printList list = mapM_ putStrLn list

readLines fileName = do
  parsedFile <- readFile fileName
  let fileLines = lines parsedFile
  let headOnly = fmap head fileLines
  let lineWords = fmap words fileLines --List of Lists broken by line
  print lineWords



-- parseTextLine xs = do
--   let y = mapM_ head xs
--   mapM_ putStrLn y


-- main = mainWith myFunction
--   where mainWith function = do
--           args <- getArgs
--           case args of
--             [input] -> printFirst function input
--         myFunction = id

main = do
  args <- getArgs
  case args of
    [input] -> readLines input



-- Instead of putStrLn, use a custom function that takes in the line, returns
-- the first index point printed
