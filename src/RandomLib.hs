module RandomLib
( Result(..)
, isOk
, isErr
, fromOk
, fromErr
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

foldlV f lst = foldl f [] lst