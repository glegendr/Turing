module JsonReader (readJson) where

import System.IO  
import Data.List
import Debug.Trace
import Turing

data JsonData = JsonStr {name :: String, value :: String} | JsonTab {name :: String, tabValue :: [JsonData]} | JsonValue {value :: String} deriving (Show)

jsonDataToString :: JsonData -> String
jsonDataToString (JsonStr b a) = b ++ " : " ++ a ++ "\n"
jsonDataToString (JsonValue x) = x ++ "\n"
jsonDataToString (JsonTab b a) = b ++ "\n{" ++ foldl1 (++) (map jsonDataToString a) ++ "}\n"

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

readJson path str = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    -- putStrLn contents
    let myData = stringToJsonData $ foldl1 (++) $ words contents
    -- applyTab putStrLn $ getJsonNames myData
    let name = (init . tail) $ head $ getNameValue "\"name\"" myData
    let alphabet = foldl1 (++) $ map (init . tail) $ getNameValue "\"alphabet\"" myData
    let blank = (init . tail) $ head $ getNameValue "\"blank\"" myData
    let states = getNameValue "\"states\"" myData
    let newState = map (init . tail) states
    let initial = (init . tail) $ head $ getNameValue "\"initial\"" myData
    let finals = map (init . tail) $ getNameValue "\"finals\"" myData
    let transitions = map (map (init . tail)) $ foldl1 (++) $ map (\x -> map (x:) $ divide 4 $ getNameValue x myData) states
    let md = newMachineDescription name alphabet blank states initial finals (map newTransitionLst transitions)
    let turing = newTuring md str
    putStrLn $ describe turing
    hClose handle
    return (turing)

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
    | otherwise = foldl1 (++) $ map (getNameValue name) v
getNameValue name (JsonStr n v)
    | name == n = [v]
getNameValue _ _ = []


getValue :: JsonData -> [String]
getValue (JsonTab _ v) = foldl1 (++) $ map getValue v
getValue j = [value j]