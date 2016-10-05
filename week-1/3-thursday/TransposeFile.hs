import System.Environment (getArgs)

-- I feel as if the series of variable assignments below could be condensed
-- into a better function chain. Attempt to refactor after Chapter completion.
readLines fileName outFile = do
  parsedFile <- readFile fileName
  let fileLines = lines parsedFile
  let lineWords = fmap words fileLines --List of Lists broken by line
  let brokenDown = concat lineWords -- remove outter list layer
  let zippedResult = zip (brokenDown !! 0) (brokenDown !! 1) -- zip contents
  -- take tuples from zipped contents & merge together as string and add newline.
  let testResult = concatMap (\(a,b) -> [a,b] ++ "\n") zippedResult
  writeFile outFile testResult

main = do
  args <- getArgs
  case args of
    [input, output] -> readLines input output
    _ -> putStrLn "error: exactly two arguments needed"
