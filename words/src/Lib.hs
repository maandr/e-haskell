module Lib
    ( grid
    , languages
    , findWord
    , findWordInLine
    , formatGrid
    , renderGrid ) where

import Data.List

type Grid = [String]

renderGrid :: IO ()
renderGrid = putStrLn (formatGrid grid)

formatGrid :: Grid -> String
formatGrid = unlines

findWord :: Grid -> String -> Bool
findWord gird word = or (map (findWordInLine word) grid)

findWordInLine :: String -> String -> Bool
findWordInLine word line = word `isInfixOf` line
--findWordInLine = isInfixOf

grid :: Grid
grid = [  "__C________R___"
        , "__SI________U__"
        , "__HASKELL____B_"
        , "__A__A_____S__Y"
        , "__R___B___C____"
        , "__PHP____H_____"
        , "____S_LREP_____"
        , "____I__M_Y__L__"
        , "____L_E__T_O___"
        , "_________HB____"
        , "_________O_____"
        , "________CN_____"
        ]

languages = [ "BASIC"
            , "COBOL"
            , "CSHARP"
            , "HASKELL"
            , "LISP"
            , "PERL"
            , "PHP"
            , "PYTHON"
            , "RUBY"
            , "SCHEME"]
