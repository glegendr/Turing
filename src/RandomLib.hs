module RandomLib
( Result(..)
, isOk
, isErr
, fromOk
, fromErr
, errFromErr
, safeTail
, safeInit
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

foldlV f lst = foldl f [] lst