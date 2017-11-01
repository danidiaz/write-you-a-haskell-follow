module NanoStr.String where

import Data.String
import Data.List (isInfixOf)
import Data.Char (isDigit)

type Str = String

emptyP :: Str
emptyP = []

headP :: Str -> Str
headP (a:_) = [a]

tailP :: Str -> Str
tailP (_:as) = as

nullP :: Str -> Bool
nullP [] = True
nullP _ = False


appendP :: String -> Str -> String
appendP s p = s ++ p

elemP :: Str -> Str -> Bool
elemP c s = c `isInfixOf` s

isDigitP :: Str -> Bool
isDigitP s = isDigit (head s)

toCharP :: Str -> Char
toCharP (c : s) = c
