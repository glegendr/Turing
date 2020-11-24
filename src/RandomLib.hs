module RandomLib
( Result(..)
, isOk
, isErr
, fromOk
, fromErr
, errFromErr
, safeTail
, safeInit
, safeHead
, replace
, foldlV
) where

data Result a = Ok a | Err String

isOk :: Result a -> Bool
isOk (Ok _) = True
isOk _ = False

isErr :: Result a -> Bool
isErr = not . isOk

fromOk :: Result a -> a
fromOk (Ok x) = x

fromErr :: Result a -> String
fromErr (Err x) = x

errFromErr :: Result a -> Result b
errFromErr res = Err (fromErr res)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

safeInit :: [a] -> [a]
safeInit [] = []
safeInit x = init x

replace :: (a -> Bool) -> a -> [a] -> [a]
replace _ _ [] = []
replace f rep (x:xs)
    | f x = rep : replace f rep xs
    | otherwise = x : replace f rep xs

safeHead [] = []
safeHead (x:_) = x

foldlV f lst = foldl f [] lst