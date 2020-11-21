module Turing
( Turing(..)
, State
, Direction
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
                                             } deriving (Show)

data Turing = Turing { tuDesc :: MachineDescription
                     , tuBef :: String
                     , tuAft :: String
                     , tuCurState :: State
                     } deriving (Show)  

isFinished :: Turing -> Bool
isFinished myTuring = cs `elem` finalStates
    where
        cs = tuCurState myTuring
        finalStates = mdFinalStates $ tuDesc myTuring

describe :: Turing -> String
describe (Turing md _ _ _) =
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
turingToString (Turing md b a cs) = show md ++ show b ++ show (take 15 a) ++ show cs 

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
newMachineDescription n a b as is fs t = MachineDescription n a (head b) as is fs t (maximum ((length "Transitions:"): map length as) + 6)

newTuring :: MachineDescription -> String -> Turing
newTuring md str = Turing md [] (str ++ repeat (mdBlank md)) (mdInitState md)

transitionToString :: Int -> Transition -> String
transitionToString space (Transition cs cc ts tc d)= "(" ++ cs ++ ", " ++ [cc] ++ ") " ++ replicate (space - length cs - 7) '.' ++ " (" ++ ts ++ ", " ++ [tc] ++ ", " ++ show d ++ ")"

getBandString :: Turing -> String
getBandString (Turing _ b (a:as) _) = "[" ++ b ++ "<" ++ [a] ++ ">" ++ take (15 - (length b + 1)) as ++ "]"

makeTransitionString :: Turing -> Result (Turing, String)
makeTransitionString t
    | trans == newTransition "" "." "" "." "left" = Err ("There is no transition adapted to this case\nBand:  " ++ getBandString newT ++ "\nState: " ++ tuCurState newT)
    | otherwise = Ok (newT, getBandString t ++ "  " ++ transitionToString (mdMaxSize $ tuDesc t)trans)
    where (newT, trans) = makeTransition t

makeTransition :: Turing -> (Turing, Transition)
makeTransition myTur@(Turing desc bef aft curState) = (Turing desc newBef newAft newState, tr)
    where 
        trans = mdTrasitions desc
        ((newBef, newAft, newState), tr) = makeTransition' bef aft curState trans
        makeTransition' :: String -> String -> State -> [Transition] -> ((String, String, State), Transition)
        makeTransition' b a cs [] = ((b, a, cs), newTransition "" "." "" "." "left")
        makeTransition'  b aft@(a:as) cs (x:xs)
            | trCurState x == cs && trCurChar x == a = (applyTransiton b aft cs x, x)
            | otherwise = makeTransition' b aft cs xs


applyTransiton :: String -> String -> State -> Transition -> (String, String, State)
applyTransiton b (_:as) _ (Transition _ _ ts tc DRight) = (b ++ [tc], as, ts)
applyTransiton [] a cs _ = ([], a, cs)
applyTransiton b (_:as) _ (Transition _ _ ts tc _) = (init b, last b : tc : as, ts)