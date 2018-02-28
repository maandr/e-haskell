module Lists where

import Control.Monad

-- strings are lists of chars
charList :: [Char]
charList = ['H', 'e', 'l', 'l', 'o']

-- double quote notation is syntactic sugar
string :: [Char]
string = "Hello"

-- list of integers
intList :: [Int]
intList = [1, 2, 3, 4, 5]

-- list of fractionals
floatList :: [Float]
floatList = [1.0, 2.0, 3.0]

-- determenting the length of a list
lengthOfString :: Int
lengthOfString = length "Hello"
    
-- take returns the first element of a list
firstElementOfInts = head [1, 2, 3, 4, 5]

-- tail chops of the head of a list and returns the remainder of the list
lastElementOfInts = tail [1, 2, 3, 4, 5]

-- list ranges
n :: [Int]
n = [0..9]

-- infinite lists
take10 :: [Int]
take10 = take 10 [0..]

isEvan :: Int -> Bool
isEvan x = x `mod` 2 == 0

-- list monad notation
filtered = do
    i <- [0..10]
    guard (isEvan i) -- predicate
    return i

-- list comprehention
nums = [i | i <- [0..100], isEvan(i)]

evanNumbers n = [i | i <- [0..n], isEvan(i)]