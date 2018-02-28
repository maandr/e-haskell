module CurryingSpec where

import Test.Hspec

-- adds two ints together
add :: Int -> Int -> Int
add a b = a + b

-- re-using a partially applied function
-- can also be written as add5 x = add 5 x
add5 :: Int -> Int
add5 = add 5

-- composing two functions
add10 :: Int -> Int
add10 = add5 . add5

spec :: Spec
spec = do

    describe "Partial application" $ do
        it "Should add 5 to a given literal" $ do
            add5 5 `shouldBe` 10
        it "Should add 10 to a given literal" $ do
            add10 10 `shouldBe` 20