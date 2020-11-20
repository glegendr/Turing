module Turing
( Turing
, State
, Direction
, MachineDescription
, isFinished
, newTransition
, newMachineDescription
, newTuring
, makeTransition
, makeTransitionString
, getBandString
, turingToString) where

import Result

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


turingToString :: Turing -> String
turingToString (Turing md b a cs) = show md ++ show b ++ show (take 15 a) ++ show cs 

directionFromString :: String -> Direction
directionFromString "Right" = DRight
directionFromString _ = DLeft

newTransition :: State -> Char -> State -> Char -> String -> Transition
newTransition cs cc ts tc d = Transition cs cc ts tc (directionFromString d)

newMachineDescription :: String -> String -> Char -> [State] -> State -> [State] -> [Transition] -> MachineDescription
newMachineDescription n a b as is fs t = MachineDescription n a b as is fs t

newTuring :: MachineDescription -> String -> String -> State -> Turing
newTuring md b a cs = Turing md b a cs

transitionToString :: Transition -> String
transitionToString (Transition cs cc ts tc d)= "(" ++ cs ++ ", " ++ [cc] ++ ") -> (" ++ ts ++ ", " ++ [tc] ++ ", " ++ show d ++ ")"

getBandString :: Turing -> String
getBandString (Turing _ b (a:as) _) = "[" ++ b ++ "<" ++ [a] ++ ">" ++ take (15 - (length b + 1)) as ++ "]"

makeTransitionString :: Turing -> Result (Turing, String)
makeTransitionString t
    | trans == newTransition "" '.' "" '.' "" = Err ("There is no transition adapted to this case\nBand:  " ++ getBandString newT ++ "\nState: " ++ tuCurState newT)
    | otherwise = Ok (newT, getBandString t ++ "  " ++ transitionToString trans)
    where (newT, trans) = makeTransition t

makeTransition :: Turing -> (Turing, Transition)
makeTransition myTur@(Turing desc bef aft curState) = (Turing desc newBef newAft newState, tr)
    where 
        trans = mdTrasitions desc
        ((newBef, newAft, newState), tr) = makeTransition' bef aft curState trans
        makeTransition' :: String -> String -> State -> [Transition] -> ((String, String, State), Transition)
        makeTransition' b a cs [] = ((b, a, cs), newTransition "" '.' "" '.' "")
        makeTransition'  b aft@(a:as) cs (x:xs)
            | trCurState x == cs && trCurChar x == a = (applyTransiton b aft cs x, x)
            | otherwise = makeTransition' b aft cs xs


applyTransiton :: String -> String -> State -> Transition -> (String, String, State)
applyTransiton b (_:as) _ (Transition _ _ ts tc DRight) = (b ++ [tc], as, ts)
applyTransiton [] a cs _ = ([], a, cs)
applyTransiton b (_:as) _ (Transition _ _ ts tc _) = (init b, last b : tc : as, ts)