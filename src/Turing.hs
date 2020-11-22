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
, getBandString
, turingToString) where

import RandomLib
import Data.Char
import qualified Data.Set as Set

type State = String

data Direction = DRight | DLeft deriving (Show, Eq)

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

turingToString :: Turing -> String
turingToString (Turing md b a cs _) = show md ++ show b ++ show (take 15 a) ++ show cs 

directionFromString :: String -> Direction
directionFromString s
    | newS `elem` ["right", "\"right\""] = DRight
    | newS `elem` ["left", "\"left\""] = DLeft
    where newS = map toLower s

newTransitionLst :: [String] -> Transition
newTransitionLst (cs:d:cc:ts:tc:_) = newTransition cs cc ts tc d

newTransition :: State -> String -> State -> String -> String -> Transition
newTransition cs cc ts tc d = Transition cs (head cc) ts (head tc) (directionFromString d)

newMachineDescription :: String -> String -> String -> [State] -> State -> [State] -> [Transition] -> MachineDescription
newMachineDescription n a b as is fs t = MachineDescription n a (head b) as is fs t (maximum ((length "Transitions:"): map length as) + 6) 0 (not $ '_' `elem` a)

newTuring :: MachineDescription -> String -> Turing
newTuring md str = Turing (md {mdBandSize = len}) [] tape state (Set.singleton ([], take (len + 2) tape, state))
    where
        state = mdInitState md
        len = length str
        tape = (str ++ repeat (mdBlank md))

transitionToString :: Int -> Transition -> String
transitionToString space (Transition cs cc ts tc d)= "(" ++ cs ++ ", " ++ [cc] ++ ") " ++ replicate (space - length cs - 7) '.' ++ " (" ++ ts ++ ", " ++ [tc] ++ ", " ++ show d ++ ")"

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
        ret = makeTransition' bef aft curState trans (mdGenericChar desc)
        ((newBef, newAft, newState), tr) = fromOk ret
        makeTransition' :: String -> String -> State -> [Transition] -> Bool -> Result ((String, String, State), Transition)
        makeTransition' b _ _ [] _ = Err "There is no transition adapted to this case"
        makeTransition'  b aft@(a:as) cs (x:xs) generic
            | trCurState x == cs && (trCurChar x == a || (trCurChar x == '_' && generic)) && isOk ret = Ok (fromOk ret, x)
            | trCurState x == cs && trCurChar x == a = errFromErr ret
            | otherwise = makeTransition' b aft cs xs generic
            where ret = applyTransiton b aft cs x

applyTransiton :: String -> String -> State -> Transition -> Result (String, String, State)
applyTransiton b (a:as) _ (Transition _ _ ts tc DRight)
    | tc == '_' = Ok (b ++ [a], as, ts)
    | otherwise = Ok (b ++ [tc], as, ts)
applyTransiton [] a cs x = Err $ "Reader head stuck in the leftmost character of the tape\nTransition: " ++ transitionToString (length (trCurState x) + 9) x
applyTransiton b (a:as) _ (Transition _ _ ts tc _)
    | tc == '_' = Ok (init b, last b : a : as, ts)
    | otherwise = Ok (init b, last b : tc : as, ts)