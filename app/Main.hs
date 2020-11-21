module Main where

import Turing
import RandomLib
import MyError
import JsonReader
import System.Environment
import Data.List

main :: IO ()
main = do
    args <- getArgs
    let arg1 = head args
    let arg2 = head $ tail args
    (jsonFile, input) <- do
        if any (`elem` ["-h", "--help"]) args || length args /= 2
        then helper >> return ("", "")
        else if not (".json" `isSuffixOf` arg1) && (".json" `isSuffixOf` arg2)
        then return (arg2, arg1)
        else if not (".json" `isSuffixOf` arg1)
        then myError True "Given file isn't a JSON one. If he is, add \".json\" at the end" >> return ("", "")
        else return (arg1, arg2)
    turing <- readJson jsonFile input
    loopToFinish turing

helper = do
    name <- getProgName 
    putStrLn $ "usage: ./" ++ name ++ " [-h] jsonfile input\n"
    putStrLn "positional arguments:"
    putStrLn "  jsonfile .... json description of the machine\n"
    putStrLn "  input ....... input of the machine\n"
    putStrLn "optional arguments:"
    putStrLn "  -h, --help .. show this help message and exit"
    exit

loopToFinish :: Turing -> IO ()
loopToFinish myTuring
    | isFinished myTuring = putStrLn $ getBandString myTuring ++ "  " ++ tuCurState myTuring
loopToFinish myTuring
    | isErr res = myError False $ fromErr res
    | otherwise = do
        let (newTuring, str) = fromOk res
        putStrLn str
        loopToFinish newTuring 
    where res = makeTransitionString myTuring