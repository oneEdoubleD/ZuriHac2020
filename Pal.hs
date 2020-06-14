module Pal where

import Data.Char

-- What a palindrome is --

isPalandrome :: String -> Maybe Bool
isPalandrome string = isOwnReverseMaybe (rejectEmpty (normalize string))

normalize :: String -> String
normalize string =
    filter notPunctuation (filter notSpace (allLowerCase string))

rejectEmpty :: String -> Maybe String
rejectEmpty word = 
    case word of
        [] -> Nothing
        _  -> Just word

isOwnReverseMaybe :: Maybe String -> Maybe Bool
isOwnReverseMaybe maybeString = 
    case maybeString of
        Nothing -> Nothing
        Just string -> Just (isOwnReverse string)

isOwnReverse :: String -> Bool
isOwnReverse word =
    word == reverse word

allLowerCase :: String -> String
allLowerCase word = map toLower word

notSpace :: Char -> Bool
notSpace x = not (x == ' ')

notPunctuation :: Char -> Bool
notPunctuation x = not (isPunctuation x)
