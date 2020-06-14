import Pal (isPalandrome)

-- The interactive program --

main :: IO ()
main = do
    word <- getLine
    print $ verbose word

verbose :: String -> String
verbose word =
    case (isPalandrome word) of
        Nothing -> "Please enter a word."
        Just False -> "Nope!"
        Just True -> "Yep!"
