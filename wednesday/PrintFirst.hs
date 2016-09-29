import System.Environment (getArgs)

printFirst function inputFile = do
  input <- readFile inputFile
  mapM_ putStrLn (lines input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input] -> printFirst function input
        myFunction = id
