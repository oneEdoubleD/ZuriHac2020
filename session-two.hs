import Data.Char

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

isPalindrome :: String -> Bool
isPalindrome word =
    word == reverse' word

isPalindromePhrase :: String -> Bool
isPalindromePhrase phrase = 
    isPalindrome (withoutSpaces phrase)

withoutSpaces :: String -> String
withoutSpaces phrase =
    case phrase of
        [] -> []
        ' ' : remainder -> withoutSpaces remainder
        first : rest -> first : withoutSpaces rest

filter' :: (Char -> Bool) -> String -> String
filter' predicate string =
    case string of 
        [] -> []
        first : remainder -> 
            case (predicate first) of
                True -> first : filter' predicate remainder
                False -> filter' predicate remainder

-- Previous session

nonEmptyPal :: String -> Maybe Bool
nonEmptyPal word
    | word == "" = Nothing
    | otherwise  = Just (isPalindrome word)

verbose :: String -> String
verbose word =
    case (nonEmptyPal word) of
        Nothing -> "Please enter a word."
        Just False -> "Nope!"
        Just True -> "Yep!"

isPalindromeIgnoringCase :: String -> Bool
isPalindromeIgnoringCase word = 
    isPalindrome (allLowerCase word)

allLowerCase :: String -> String
allLowerCase word = map' toLower word

map' :: (a -> a) -> [a] -> [a]
map' func list = 
    case list of
        [] -> []
        (first : rest) -> func first : map' func rest

main :: IO ()
main = do
    word <- getLine
    print $ verbose word
