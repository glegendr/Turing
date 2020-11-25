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
import Data.List.Split

data JsonData = JsonStr {name :: String, value :: String} | JsonTab {name :: String, tabValue :: [JsonData]} | JsonValue {value :: String} deriving (Show)

allJson = allJsonTab ++ allJsonStr
allJsonTab = ["\"alphabet\"", "\"states\"", "\"finals\"", "\"transitions\""]
allJsonStr = ["\"name\"", "\"blank\"", "\"initial\""]
allTransitionValue = ["\"read\"", "\"to_state\"", "\"write\"", "\"action\""]

jsonDataToString :: JsonData -> String
jsonDataToString (JsonStr b a) = b ++ " : " ++ a ++ "\n"
jsonDataToString (JsonValue x) = x ++ "\n"
jsonDataToString (JsonTab b a) = b ++ "\n{" ++ foldlV (++) (map jsonDataToString a) ++ "}\n"

isGenericWellPlaced :: String -> Bool
isGenericWellPlaced [] = True
isGenericWellPlaced ('_':[]) = True
isGenericWellPlaced ('_':xs) = False
isGenericWellPlaced (_:xs) = isGenericWellPlaced xs

myWords :: String -> [String]
myWords [] = []
myWords str
    | bef == [] = myWords aft
    | otherwise = bef : myWords aft
    where (bef, aft) = spanOutString (not . isSpace) str 

spanOutString :: (Char -> Bool) -> String -> (String, String)
spanOutString _ [] = ([], [])
spanOutString f str =
    let bef = spanOutString' False f str
        aft = drop (length bef) str
    in (bef, safeTail aft)

spanOutString' :: Bool -> (Char -> Bool) -> String -> String
spanOutString' _ _ [] = []
spanOutString' b f (x:xs)
    | not (f x) && not b = []
    | x == '\"' = x : spanOutString' (not b) f xs
    | otherwise = x : spanOutString' b f xs

readJson path str = do
    handle <- catch (openFile path ReadMode >>= (\x -> return (Just x))) (\(_::SomeException) -> myError True ("Cannot open " ++ path) >> return Nothing)
    contents <- hGetContents (fromJust handle)
    let myData = stringToJsonData $ foldlV (++) $ myWords contents
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
    let warningTransition = map fst $ filter ((== False) . snd) $ map (\x -> ((safeInit . safeTail) x, isGenericWellPlaced $ foldlV (++) $ map (safeInit . safeTail) $ foldlV (++) $ map (safeInit . safeInit . safeTail) $ divide 4 $ getNameValue x rawTrans)) states
    let transitions = map (map (safeInit . safeTail)) $ foldlV (++) $ map (\x -> map (x:) $ divide 4 $ getNameValue x rawTrans) states
    let genericVariable = stringToBool $ (safeInit . safeTail) $ safeHead $ getNameValue "\"generic_variable\"" myData
    let genericFunction = stringToBool $ (safeInit . safeTail) $ safeHead $ getNameValue "\"generic_transitions\"" myData
    applyTab (checkTransition (genericVariable, genericFunction) 0 alphabet newState) transitions
    let md = newMachineDescription machineName alphabet blank newState initial finals (map newTransitionLst transitions) genericVariable genericFunction
    let turing = newTuring md str
    checkTuring str turing
    case ('_' `elem` alphabet, warningTransition) of
        (True, _) -> return ()
        (_, []) -> return ()
        (_, wt) -> myWarning $ "Transitions will be skipped due to generic character in {" ++ (init $ tail $ foldlV (++) $ map (\z -> " " ++ z ++ ",") wt) ++ "}"
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

checkTransition :: (Bool, Bool) -> Int -> String -> [String] -> [String] -> IO ()
checkTransition _ _ _ _ [] = return ()
checkTransition _ _ _ _ ([]:_) = myError True "Transition not well formated. Empty value"
checkTransition (gv, gf) i alpha states (x:xs)
    | (i == 0 || (i == 3 && not gf)) && x `notElem` states = myError True $ "Unknown state \"" ++ x ++ "\""
    | i == 3 && any (`notElem` (states ++ [[], "X", "Y", "F", "D", "RIGHT", "LEFT", "CURR"] ++ (map (\x -> [x]) alpha))) (splitOn "_" x) && gf = myError True $ "Generic state \"" ++ x ++ "\" not well fromated"
    | i `elem` [2, 4] && length x /= 1 = myError True "Multiple character in \"read\" and/or \"write\" tag"
    | i `elem` [2, 4] && (head x) `elem` "YX" && (head x) `notElem` alpha && not gf = myError True $ "\"" ++ x ++ "\" can only be used if Generic Functions are enable or if he is specified in the alphabet"
    | i `elem` [2, 4] && (head x) `notElem` (alpha ++ "YX") && not (head x == '_' && gv) =  myError True $ "\"" ++ x ++ "\" isn't in the alphabet"
checkTransition (_, gf) 1 _ _ (x:xs)
    | newS == "d" && not gf = myError True "Direction \"D\" can only be used if Generic Functions are enable"
    | newS `notElem` ["right", "left", "d"] = myError True $  "Unkown directrion \"" ++ x ++ "\" in action tag"
    where newS = map toLower x
checkTransition g i a s (_:xs) = checkTransition g (i + 1) a s xs

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
    let bef = spanOut' False 0 f str
        aft = drop (length bef) str
    in (bef, aft)

spanOut' :: Bool -> Int -> (Char -> Bool) -> String -> String
spanOut' _ _ _ [] = []
spanOut' b i f (x:xs)
    | not (f x) && i == 0 = []
    | x == '\"' = x : spanOut' (not b) i f xs
    | x `elem` "[{" && not b = x : spanOut' b (i + 1) f xs
    | x `elem` "}]" && not b = x : spanOut' b (i - 1) f xs
    | otherwise = x : spanOut' b i f xs

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