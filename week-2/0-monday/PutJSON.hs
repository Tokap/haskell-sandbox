putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)
