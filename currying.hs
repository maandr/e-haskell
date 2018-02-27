module Curring where

add :: Num a => a -> a -> a
add a b = a + b

plus10 :: Int -> Int
plus10 = add 10

result :: Int
result = plus10 10