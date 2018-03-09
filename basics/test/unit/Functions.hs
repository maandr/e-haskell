module Functions where

import Test.Hspec

allEqual :: (Eq a, Num a) => a -> a -> a -> Bool
allEqual x y z = x == y && y == z

applyAndConcat :: ([a] -> b) -> ([a] -> b) -> [a] -> [b]
applyAndConcat f g x = (f x) : (g x) : []

sumUp :: [Int] -> Int 
sumUp n = foldl (+) 0 n

calcOver :: [Int] -> (Int -> Int -> Int) -> Int
calcOver n f = foldl f 0 n

spec = do

    describe "sumUp" $ do
        it "should sum a list of integers" $ do
            sumUp [1, 2, 3] `shouldBe` 6

    describe "calcOver" $ do
        it "should apply a link operation over all integers in a list" $ do
            calcOver [1, 2, 3] (+) `shouldBe` 6
            calcOver [1, 2, 3] (-) `shouldBe` -4
            calcOver [1, 2, 3] (*) `shouldBe` 6

    describe "allEqual" $ do
        it "should compare tree numbers for equality" $ do
            allEqual 2 2 2 `shouldBe` True
            allEqual 2 2 3 `shouldBe` False
            allEqual 2 3 2 `shouldBe` False
            allEqual 3 2 2 `shouldBe` False

    describe "applyAndConcat" $ do
        it "should apply passed functions and concatinate the result" $ do
            applyAndConcat head last "Hello" `shouldBe` "Ho"
            applyAndConcat head last [1, 2, 3, 4] `shouldBe` [1, 4]
            applyAndConcat init tail "Hello" `shouldBe` ["Hell", "ello"]