module MyError
    ( myError
    , exit
    , myWarning
    ) where

import System.Exit
import Rainbow
import Data.Function ((&))
import Data.Text (pack)

jsonErrToStr :: Bool -> String
jsonErrToStr True = "JSON "
jsonErrToStr _ = ""

myError :: Bool -> String -> IO ()
myError b str = do
    let myErr = lines str
    putChunkLn $ (chunk $ pack $ jsonErrToStr b ++ "ERROR: " ++ (head myErr)) & fore red & bold
    case (tail myErr) of
        [] -> return ()
        x -> putStrLn $ init $ foldl1 (++) $ map (++ "\n") x
    exitWith (ExitFailure $ length str)

myWarning :: String -> IO ()
myWarning str = do
    let myWarn = lines str
    putChunkLn $ (chunk $ pack $ "WARNING: " ++ (head myWarn)) & fore yellow & bold
    case (tail myWarn) of
        [] -> return ()
        x -> putStrLn $ init $ foldl1 (++) $ map (++ "\n") x

exit = exitWith ExitSuccess