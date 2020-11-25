module Turing
( Turing(..)
, MachineDescription(..)
, describe
, isFinished
, newTransition
, newTransitionLst
, newMachineDescription
, newTuring
, makeTransition
, makeTransitionString
, getBandString) where

import RandomLib
import Data.Char
import qualified Data.Set as Set
import Data.List.Split
import Data.List
import Debug.Trace

type State = String

data Direction = DRight | DLeft | DUndeterminated deriving (Show, Eq)

data Transition = Transition { trCurState :: State
                             , trCurChar :: Char
                             , trToState :: State
                             , trToChar :: Char
                             , trDir :: Direction
                             } deriving (Show, Eq)  

data MachineDescription = MachineDescription { mdName :: String
                                             , mdAlphabet :: String
                                             , mdBlank :: Char
                                             , mdAllStates :: [State]
                                             , mdInitState :: State
                                             , mdFinalStates :: [State]
                                             , mdTrasitions :: [Transition]
                                             , mdMaxSize :: Int
                                             , mdBandSize :: Int
                                             , mdGenericChar :: Bool
                                             , mdGenericFunc :: Bool
                                             } deriving (Show)

data Turing = Turing { tuDesc :: MachineDescription
                     , tuBef :: String
                     , tuAft :: String
                     , tuCurState :: State
                     , tuSet :: Set.Set (String, String, State)
                     } deriving (Show)  

isFinished :: Turing -> Bool
isFinished myTuring = cs `elem` finalStates
    where
        cs = tuCurState myTuring
        finalStates = mdFinalStates $ tuDesc myTuring

describe :: Turing -> String
describe (Turing md _ _ _ _) =
    let space = mdMaxSize md
        name = "Name: " ++ replicate (space - length "Name:  ") '.' ++ " " ++ mdName md ++ "\n"
        alphabet = "Alphabet: " ++ replicate (space - length "Alphabet:  ") '.' ++ " [" ++ (init $ tail $ foldlV (++) $ map (\x -> " " ++ [x] ++ ",") $ mdAlphabet md) ++ "]\n"
        blank = "Blank: " ++ replicate (space - length "Blank:  ") '.' ++ " " ++ [mdBlank md] ++ "\n"
        states = "States: " ++ replicate (space - length "States:  ") '.' ++ " [" ++ (init $ tail $ foldlV (++) $ map (\x -> " " ++ x ++ ",") $ mdAllStates md) ++ "]\n"
        initial = "Initial: " ++ replicate (space - length "Initial:  ") '.' ++ " " ++ mdInitState md ++ "\n"
        final = "Finals: " ++ replicate (space - length "Finals:  ") '.' ++ " [" ++ (init $ tail $ foldlV (++) $ map (\x -> " " ++ x ++ ",") $ mdFinalStates md) ++ "]\n"
        transitions = "Transitions:\n" ++ (foldlV (++) $ map ((++ "\n") . transitionToString space) $ mdTrasitions md)
    in name ++ alphabet ++ blank ++ states ++ initial ++ final ++ transitions

directionFromString :: String -> Direction
directionFromString s
    | newS `elem` ["right", "\"right\""] = DRight
    | newS `elem` ["left", "\"left\""] = DLeft
    | otherwise = DUndeterminated
    where newS = map toLower s

dirToString :: Direction -> String
dirToString DRight = "RIGHT"
dirToString DLeft = "LEFT"
dirToString _ = "UNDEFINED"

newTransitionLst :: [String] -> Transition
newTransitionLst (cs:d:cc:ts:tc:_) = newTransition cs cc ts tc d

newTransition :: State -> String -> State -> String -> String -> Transition
newTransition cs cc ts tc d = Transition cs (head cc) ts (head tc) (directionFromString d)

newMachineDescription :: String -> String -> String -> [State] -> State -> [State] -> [Transition] -> Bool -> Bool -> MachineDescription
newMachineDescription n a b as is fs t gc gf = MachineDescription n a (head b) as is fs t (maximum ((length "Transitions:"): map length as) + 9) 0  gc gf

newTuring :: MachineDescription -> String -> Turing
newTuring md str = Turing (md {mdBandSize = len}) [] tape state (Set.singleton ([], take (len + 2) tape, state))
    where
        state = mdInitState md
        len = length str
        tape = (str ++ repeat (mdBlank md))

transitionToString :: Int -> Transition -> String
transitionToString space (Transition cs cc ts tc d)= "(" ++ cs ++ ", " ++ [cc] ++ ") " ++ replicate (space - length cs - 7) '.' ++ " (" ++ ts ++ ", " ++ [tc] ++ ", " ++ dirToString d ++ ")"

getBandString :: Turing -> String
getBandString (Turing md b (a:as) _ _) = "[" ++ b ++ "<" ++ [a] ++ ">" ++ take ((mdBandSize md + 2) - (length b + 1)) as ++ "]"

makeTransitionString :: Turing -> Result (Turing, String)
makeTransitionString t
    | isErr ret = errFromErr ret
    | otherwise = Ok (newT, getBandString t ++ "  " ++ transitionToString (mdMaxSize $ tuDesc t)trans)
    where
        ret = makeTransition t
        (newT, trans) = fromOk ret

delLastBlank :: Char -> String -> String
delLastBlank c str = reverse $ dropWhile (== c) $ reverse str

makeTransition :: Turing -> Result (Turing, Transition)
makeTransition myTur@(Turing desc bef aft curState set)
    | isErr ret = Err $ fromErr ret ++ "\nTape:  " ++ getBandString myTur ++ "\nState: " ++ tuCurState myTur
    | Set.member newT set = Err $ "Loop detected" ++ "\nTape: " ++ getBandString myTur ++ "\nNew Tape: "++ getBandString newTur ++ "\nState: " ++ tuCurState myTur ++ "\nTransition: " ++ transitionToString (len + 2) tr
    | otherwise = Ok (newTur, tr)
    where 
        newTur = Turing desc newBef newAft newState (Set.insert newT set)
        blank = mdBlank desc
        newT = (delLastBlank blank newBef, take (len + 2) newAft, newState)
        len = mdBandSize desc
        trans = mdTrasitions desc
        ret = makeTransition' bef aft curState trans (tuDesc myTur)
        ((newBef, newAft, newState), tr) = fromOk ret
        makeTransition' :: String -> String -> State -> [Transition] -> MachineDescription -> Result ((String, String, State), Transition)
        makeTransition' _ _ _ [] _  = Err "There is no transition adapted to this case"
        makeTransition'  b aft@(a:as) cs (x:xs) md
            | genericF && safeHead splited == "READ" && safeHead splitedTrans == "READ" && (transCC == a || (transCC == '_' && generic)) = Ok ((rb, ra, rs), rt)
            | trCurState x == cs && (transCC == a || (transCC == '_' && generic)) && isOk ret = Ok (fromOk ret, x)
            | trCurState x == cs && transCC == a = errFromErr ret
            | genericF && myFunc == trCurState x && trCurState x /= [] &&(transCC == a || (transCC == '_' && generic) || (myX == a && transCC == 'X') || (myY == a && transCC == 'Y')) && isOk specialRet = Ok ((sb, sa, ss), st)
            | genericF && myFunc == trCurState x && trCurState x /= [] && (transCC == a || (transCC == '_' && generic) || (myX == a && transCC == 'X') || (myY == a && transCC == 'Y')) = errFromErr specialRet
            | otherwise = makeTransition' b aft cs xs md
            where
                generic = mdGenericChar md
                genericF = mdGenericFunc md
                transCC = trCurChar x
                splited = splitOn "_" cs
                (myX, myFunc, myDir, myY, myRest) = getFromSplited splited md
                splitedTrans = splitOn "_" (trCurState x)
                ret = applyTransiton b aft x
                (sb, sa, ss, st) = fromOk specialRet
                specialRet = applySpecialTrans b aft myX myDir myY myRest x
                (rb, ra, rs, rt) = makeRead b aft (safeTail splited) x

toSafeSplitted :: Char -> Int -> [String] -> [String]
toSafeSplitted b 0 x
    | x == [] = [b] : toSafeSplitted b 1 []
    | head x == [] = [b] : toSafeSplitted b 1 (safeTail x)
    | otherwise = head x : toSafeSplitted b 1 (safeTail x)
toSafeSplitted b 1 x = safeHead x : toSafeSplitted b 2 (safeTail x)
toSafeSplitted b 2 x
    | x == [] = "RIGHT" : toSafeSplitted b 3 []
    | head x == "RIGHT" = head x : toSafeSplitted b 3 (safeTail x)
    | head x == "LEFT" = head x : toSafeSplitted b 3 (safeTail x)
    | otherwise = "RIGHT" : toSafeSplitted b 3 x
toSafeSplitted b 3 x
    | length (safeHead x) == 1 = head x : toSafeSplitted b 4 (safeTail x)
    | otherwise = [b] : toSafeSplitted b 4 (safeTail x)
toSafeSplitted _ _ x = x

getFromSplited :: [String] -> MachineDescription -> (Char, String, Direction, Char, [String])
getFromSplited splited md =
    let safeSplitted = toSafeSplitted (mdBlank md) 0 splited
        (myX:myFunc:myDir:myY:myRest) = safeSplitted
    in ((head myX), myFunc, (directionFromString myDir), (head myY), myRest)


makeRead :: String -> String -> [String] -> Transition -> (String, String, State, Transition)
makeRead b (a:as) oldFunc t@(Transition _ _ ts tc _)
    | tc == '_' = (b ++ [a], as, newTs, t {trToState = newTs})
    | otherwise = (b ++ [tc], as, newTs, t {trToState = newTs})
    where
        newTs = intercalate "_" $ filter (/= []) $ replace (== "CURR") [a] $ replace (== "F") (intercalate "_" oldFunc) $ splitOn "_" ts

applySpecialTransRight b (a:as) myX myY rest t@(Transition _ _ ts tc _)
    | tc == '_' = Ok (b ++ [a], as, newTs, t {trToState = newTs})
    | tc == 'X' = Ok (b ++ [myX], as, newTs, t {trToState = newTs})
    | tc == 'Y' = Ok (b ++ [myY], as, newTs, t {trToState = newTs})
    | otherwise = Ok (b ++ [tc], as, newTs, t {trToState = newTs})
    where
        newTs = intercalate "_" $ replace (== "CURR") [a] $ replace (== "D") "RIGHT" $ replace (== "Y") [myY] $ replace (== "X") [myX] $ replace (== "F") (intercalate "_" rest) $ splitOn "_" ts

applySpecialTransLeft b (a:as) myX myY rest t@(Transition _ _ ts tc _)
    | tc == '_' = Ok (init b, last b : a : as, newTs, t {trToState = newTs})
    | tc == 'X' = Ok (init b, last b : myX : as, newTs, t {trToState = newTs})
    | tc == 'Y' = Ok (init b, last b : myY : as, newTs, t {trToState = newTs})
    | otherwise = Ok (init b, last b : tc : as, newTs, t {trToState = newTs})
    where
        newTs = intercalate "_" $ replace (== "CURR") [a] $ replace (== "D") "LEFT" $ replace (== "Y") [myY] $ replace (== "X") [myX] $ replace (== "F") (intercalate "_" rest) $ splitOn "_" ts

applySpecialTrans :: String -> String -> Char -> Direction -> Char -> [String] -> Transition -> Result (String, String, State, Transition)
applySpecialTrans [] _ _ dir _ _ t@(Transition _ _ ts tc td)
    | (dir == DLeft && td == DUndeterminated) || td == DLeft =  Err $ "Reader head stuck in the leftmost character of the tape\nTransition: " ++ transitionToString (length (trCurState t) + 9) t
applySpecialTrans b a myX _ myY rest t@(Transition _ _ ts tc DRight) = applySpecialTransRight b a myX myY rest t
applySpecialTrans b a myX _ myY rest t@(Transition _ _ ts tc DLeft) = applySpecialTransLeft b a myX myY rest t
applySpecialTrans b a myX DRight myY rest t@(Transition _ _ ts tc _) = applySpecialTransRight b a myX myY rest t
applySpecialTrans b a myX _ myY rest t@(Transition _ _ ts tc _) = applySpecialTransLeft b a myX myY rest t

applyTransiton :: String -> String -> Transition -> Result (String, String, State)
applyTransiton b (a:as) (Transition _ _ ts tc DRight)
    | tc == '_' = Ok (b ++ [a], as, ts)
    | otherwise = Ok (b ++ [tc], as, ts)
applyTransiton [] _ x = Err $ "Reader head stuck in the leftmost character of the tape\nTransition: " ++ transitionToString (length (trCurState x) + 9) x
applyTransiton b (a:as) (Transition _ _ ts tc _)
    | tc == '_' = Ok (init b, last b : a : as, ts)
    | otherwise = Ok (init b, last b : tc : as, ts)