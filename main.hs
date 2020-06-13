spell :: Integer -> String
spell int = 
    case int of
        1 -> "one"
        2 -> "two"
        3 -> "three"
        4 -> "four"
        _ -> "I don't know this number!"

spell' x 
    | x == 1 = "one"
    | x == 2 = "two"
    | otherwise = "dunno"

ifLessThanTen :: Integer -> Integer
ifLessThanTen x 
    | x < 10 = x * (-1)
    | otherwise = x + 10

idk :: (Num a, Ord a) => a -> a
idk x = 
    if (x < 10) then (negate x) else (x + 10)

idk' :: (Num a, Ord a) => a -> a
idk' x = 
    case (x < 10) of 
        True -> negate x
        False -> x + 10

preferJ x y = 
    if (elem 'j' x) then x else y
