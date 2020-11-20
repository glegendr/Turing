module MyError
    ( myError
    ) where

import System.Exit

myError :: String -> IO ()
myError str = do
    putStrLn $ "ERROR: " ++ str
    exitWith (ExitFailure $ length str)