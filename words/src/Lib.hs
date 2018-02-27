module Lib
    ( Grid
    , getPossibilities
    , skew
    , findWord
    , findWords
    , findWordInLine
    , formatGrid
    , renderGrid ) where

import Data.List (isInfixOf, transpose)
import Data.Maybe (catMaybes)

type Grid = [String]

renderGrid :: Grid -> IO ()
renderGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid -> String
formatGrid = unlines

getPossibilities :: Grid -> [String]
getPossibilities grid = 
    let horizontal = grid
        vertical = transpose grid
        diagonal1 = diagonalize grid
        diagonal2 = diagonalize (map reverse horizontal)
        allLines = horizontal ++ vertical ++ diagonal1 ++ diagonal2
    in allLines  ++ (map reverse allLines)

-- dot notation . composing transpose and skew 
diagonalize :: Grid -> Grid
diagonalize = transpose . skew

skew :: Grid -> Grid
skew [] = []
skew (l:ls) = l : skew(map indent ls)
    where indent line = '_' : line

findWord :: Grid -> String -> Maybe String
findWord grid word =
    let possibilities = getPossibilities grid
        found = or (map (findWordInLine word) possibilities)
    in if found then Just word else Nothing

findWords :: Grid -> [String] -> [String]
findWords grid words = 
    let foundWords = map(findWord grid) words
    in catMaybes foundWords

findWordInLine :: String -> String -> Bool
findWordInLine word line = word `isInfixOf` line
--findWordInLine = isInfixOf
