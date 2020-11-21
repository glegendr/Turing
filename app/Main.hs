module Main where

import Turing
import RandomLib
import MyError
import JsonReader

main :: IO ()
main = do
    turing <- readJson "json.json" "111-11="
    loopToFinish turing

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