module Lists ( isEvan
             , getEvanNumbersUntilListComprehention
             , getEvanNumbersUntilMonad ) where

import Control.Monad

-- determines weather or not the given number is evan
-- predicate
isEvan :: Int -> Bool
isEvan x = x `mod` 2 == 0

-- gets all evan numbers until the given boundary is exceeded
-- use list comprehention
getEvanNumbersUntilListComprehention :: Int -> [Int]
getEvanNumbersUntilListComprehention n = [i | i <- [0..n], isEvan i]

-- gets all evan numbers until the given boundary is exceeded
-- use list monad notation
getEvanNumbersUntilMonad :: Int -> [Int]
getEvanNumbersUntilMonad n = do
    i <- [0..n]
    guard (isEvan i) -- filter
    return i -- map