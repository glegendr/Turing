module Main where

import Turing
import Result
import MyError

main :: IO ()
main = do
    let tr1 = newTransition "0" 'b' "0" 'b' "Right"
    let tr2 = newTransition "0" 'a' "1" 'a' "Right"
    let tr3 = newTransition "1" 'b' "1" 'b' "Right"
    let tr4 = newTransition "1" 'a' "0" 'a' "Right"
    let tr5 = newTransition "1" '#' "2" '#' "Right"
    let trLst = [tr1, tr2, tr3, tr4, tr5]
    let md = newMachineDescription "my Machine" "ab#" '#' ["0", "1", "2"] "0" ["2"] trLst
    let turing = newTuring md [] ("abba" ++ repeat '#') "0"
    loopToFinish turing

loopToFinish :: Turing -> IO ()
loopToFinish myTuring
    | isFinished myTuring = putStrLn $ getBandString myTuring
loopToFinish myTuring
    | isErr res = myError $ fromErr res
    | otherwise = do
        let (newTuring, str) = fromOk res
        putStrLn str
        loopToFinish newTuring 
    where res = makeTransitionString myTuring