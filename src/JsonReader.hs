{-# LANGUAGE ScopedTypeVariables #-}

module JsonReader (readJson) where

import System.IO  
import Data.List
import Debug.Trace
import Turing
import MyError
import RandomLib
import Data.Maybe
import Data.Char
import Control.Exception

data JsonData = JsonStr {name :: String, value :: String} | JsonTab {name :: String, tabValue :: [JsonData]} | JsonValue {value :: String} deriving (Show)

allJson = allJsonTab ++ allJsonStr
allJsonTab = ["\"alphabet\"", "\"states\"", "\"finals\"", "\"transitions\""]
allJsonStr = ["\"name\"", "\"blank\"", "\"initial\""]
allTransitionValue = ["\"read\"", "\"to_state\"", "\"write\"", "\"action\""]

jsonDataToString :: JsonData -> String
jsonDataToString (JsonStr b a) = b ++ " : " ++ a ++ "\n"
jsonDataToString (JsonValue x) = x ++ "\n"
jsonDataToString (JsonTab b a) = b ++ "\n{" ++ foldlV (++) (map jsonDataToString a) ++ "}\n"

readJson path str = do
    handle <- catch (openFile path ReadMode >>= (\x -> return (Just x))) (\(_::SomeException) -> myError True ("Cannot open " ++ path) >> return Nothing)
    contents <- hGetContents (fromJust handle)
    let myData = stringToJsonData $ foldlV (++) $ words contents
    checkJson myData
    let machineName = (safeInit . safeTail) $ head $ getNameValue "\"name\"" myData
    let alphabet = foldlV (++) $ map (safeInit . safeTail) $ getNameValue "\"alphabet\"" myData
    let blank = (safeInit . safeTail) $ head $ getNameValue "\"blank\"" myData
    case blank of
        [] -> myError True "Blank character is empty"
        otherwise -> return ()
    let states = getNameValue "\"states\"" myData
    let newState = map (safeInit . safeTail) states
    let initial = (safeInit . safeTail) $ head $ getNameValue "\"initial\"" myData
    let finals = map (safeInit . safeTail) $ getNameValue "\"finals\"" myData
    let rawTrans = JsonTab "" $ map (\x -> JsonTab (name x) (map (\(JsonTab zn zv) -> JsonTab zn (sortOn name zv)) (tabValue x))) $ tabValue $ fromJust $ find ((== "\"transitions\"") . name) (tabValue myData)
    let transitions = map (map (safeInit . safeTail)) $ foldlV (++) $ map (\x -> map (x:) $ divide 4 $ getNameValue x rawTrans) states
    applyTab (checkTransition 0 alphabet newState) transitions
    let md = newMachineDescription machineName alphabet blank newState initial finals (map newTransitionLst transitions)
    let turing = newTuring md str
    checkTuring str turing
    putStrLn $ describe turing
    hClose (fromJust handle)
    return (turing)

checkTuring :: String -> Turing -> IO ()
checkTuring str (Turing md _ band _ _) = do
    let alphabet = mdAlphabet md
    let blank = mdBlank md
    case foldl (&&) True (map (\x -> x `elem` alphabet && x /= blank) str) of
        True -> return ()
        False -> myError False "Wrong character founded in input"
    let initState = mdInitState md
    let allStates = mdAllStates md
    let finalStates = mdFinalStates md
    if initState `notElem` allStates
    then myError True $ "Initial state \"" ++ initState ++ " \"isn't in \"states\" tag"
    else if any (`notElem` allStates) finalStates
    then myError True "One of the final states isn't in \"states\" tag"
    else return ()
    case mdName md of
        [] -> myError True "Empty name"
        "Hitler" -> myError True "Historical Paradox: Turing coundn't name one of his creation like this"
        otherwise -> return ()
    return ()

checkTransition :: Int -> String -> [String] ->[String] -> IO ()
checkTransition _ _ _ [] = return ()
checkTransition _ _ _ ([]:_) = myError True "Transition not well formated. Empty value"
checkTransition i alpha states (x:xs)
    | i `elem` [0, 3] && x `notElem` states = myError True $ "Unknown state \"" ++ x ++ "\""
    | i `elem` [2, 4] && length x /= 1 =  myError True "Multiple character in \"read\" and/or \"write\" tag"
    | i `elem` [2, 4] && (head x) `notElem` alpha && (head x) /= '_' =  myError True $ "\"" ++ x ++ "\" isn't in the alphabet"
checkTransition 1 _ _ (x:xs)
    | newS `notElem` ["right", "left"] = myError True $  "Unkown directrion \"" ++ x ++ "\" in action tag"
    where newS = map toLower x
checkTransition i a s (_:xs) = checkTransition (i + 1) a s xs

checkJson :: JsonData -> IO ()
checkJson (JsonTab _ v) = do
    applyTab (\ x -> case x of
            JsonValue value -> myError True $ "Value " ++ value ++ " not expected" 
            otherwise -> return ()) v
    let tabNames = map name v
    case foldl (flip delete) allJson tabNames of
        [] -> return ()
        x -> myError True $ "Missing value(s) {" ++ (init $ tail $ foldlV (++) $ map (\z -> " " ++ z ++ ",") x) ++ "}"
    case checkDuplicate [] (sort tabNames) of
        [] -> return ()
        x -> myError True $ "Duplicate tag " ++ x
    applyTab (\x -> case x of
        JsonTab _ _ -> return ()
        x -> myError True $ name x ++ " must be a tab") $ filter (flip elem allJsonTab . name) v
    applyTab (\x -> case x of
        JsonStr _ _ -> return ()
        x -> myError True $ name x ++ " must be at format: " ++ name x ++ " : \"value\"") $ filter (flip elem allJsonStr . name) v
    let transitions = tabValue $ fromJust $ find ((== "\"transitions\"") . name) v
    let transNames = map value $ tabValue $ fromJust $ find ((== "\"states\"") . name) v
    applyTab (\ x -> case x of
            JsonValue value -> myError True $ "Value " ++ value ++ " not expected" 
            otherwise -> return ()) transitions
    applyTab (\x -> case x of
        JsonTab _ _ -> return ()
        x -> myError True $ name x ++ " must be a tab") $ filter (flip elem transNames . name) transitions
    case checkDuplicate [] (sort (map name transitions)) of
        [] -> return ()
        x -> myError True $ "Duplicate tag " ++ x
    case foldl (flip delete) (map name transitions) transNames of
        [] -> return ()
        x -> myError True $ "Unknow transition(s) state(s) {" ++ (init $ tail $ foldlV (++) $ map (\z -> " " ++ z ++ ",") x) ++ "}"
    case all (\(JsonTab n1 v1) -> all (\x -> case x of
            JsonTab _ v2 -> length v2 == 4 && all (\z -> case z of
                JsonStr n _ -> n `elem` allTransitionValue
                otherwise -> False) v2 && checkDuplicate [] (sort $ map name v2) == []
            otherwise -> False) v1) transitions of
        True -> return ()
        False -> myError True "Transitions not well formated"
    return ()
checkJson _ = myError True "Surround your Json data by \"{}\""

checkDuplicate :: String -> [String] -> String
checkDuplicate _ [] = []
checkDuplicate bef (x:xs)
    | bef == x = bef
    | otherwise = checkDuplicate x xs

divide :: Int -> [String] -> [[String]]
divide _ [] = []
divide i tab = take i tab : divide i (drop 4 tab)

splitOnOut :: Char -> String -> [String]
splitOnOut _ [] = []
splitOnOut c str = bef : splitOnOut c (safeTail aft)
    where (bef, aft) = spanOut (/= c) str

spanOut :: (Char -> Bool) -> String -> (String, String)
spanOut _ [] = ([], [])
spanOut f str =
    let bef = spanOut' 0 f str
        aft = drop (length bef) str
    in (bef, aft)

spanOut' :: Int -> (Char -> Bool) -> String -> String
spanOut' _ _ [] = []
spanOut' i f (x:xs)
    | not (f x) && i == 0 = []
    | x `elem` "[{" = x : spanOut' (i + 1) f xs
    | x `elem` "}]" = x : spanOut' (i - 1) f xs
    | otherwise = x : spanOut' i f xs

applyTab :: (a -> IO ()) -> [a] -> IO ()
applyTab _ [] = return ()
applyTab f (x:xs) = do
    f x
    applyTab f xs

stringToJsonData :: String -> JsonData
stringToJsonData [] = JsonValue []
stringToJsonData str
    | head str == '{' = JsonTab [] (map stringToJsonData (splitOnOut ',' $ init $ tail str))
    | aft == [] = JsonValue str
    | head aft `elem` "[{" = JsonTab bef (map stringToJsonData (splitOnOut ',' $ init $ tail aft))
    | otherwise = JsonStr bef aft
    where
        (bef, rawAft) = span (/= ':') str
        aft = safeTail rawAft

getNameValue :: String -> JsonData -> [String]
getNameValue name j@(JsonTab n v)
    | name == n = getValue j
    | otherwise = foldlV (++) $ map (getNameValue name) v
getNameValue name (JsonStr n v)
    | name == n = [v]
getNameValue _ _ = []


getValue :: JsonData -> [String]
getValue (JsonTab _ v) = foldlV (++) $ map getValue v
getValue j = [value j]