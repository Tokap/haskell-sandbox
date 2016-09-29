import System.Environment (getArgs)

printList list = mapM_ putStrLn list

-- I feel as if the series of variable assignments below could be condensed
-- into a better function chain. Attempt to refactor after Chapter completion.
readLines fileName = do
  parsedFile <- readFile fileName
  let fileLines = lines parsedFile
  let headOnly = fmap head fileLines
  let lineWords = fmap words fileLines --List of Lists broken by line
  let firstWords = fmap head lineWords --Just the first word from each line
  printList firstWords

main = do
  args <- getArgs
  case args of
    [input] -> readLines input
