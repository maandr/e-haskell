module Dictionary where

-- dictionary holding the days of the week
weekDays :: [(String, Int)]
weekDays = [
    ("sunday", 0),
    ("monday", 1),
    ("tuesday", 2),
    ("wednesday", 3),
    ("thursday", 4),
    ("friday", 5),
    ("saturday", 6)]

-- the lookup function will return the value corresponding to
-- the given key or nothing if the key is not present
lookup' :: Eq a => a -> [(a, b)] -> Maybe b
lookup' a b = lookup a b