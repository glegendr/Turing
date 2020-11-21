module MyError
    ( myError
    ) where

import System.Exit

myError :: Bool -> String -> IO ()
myError False str = do
    putStrLn $ "ERROR: " ++ str
    exitWith (ExitFailure $ length str)
myError _ str = do
    putStrLn $ "JSON ERROR: " ++ str
    exitWith (ExitFailure $ length str)