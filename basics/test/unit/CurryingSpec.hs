module CurryingSpec where

import Test.Hspec

-- curried function
curriedAdd :: Int -> Int -> Int
curriedAdd a b = a + b

-- un-curried function
uncurriedAdd :: (Int, Int) -> Int
uncurriedAdd (a, b) = a + b

-- advantage of a curried functions over a un-curried function
-- is that is can be re-used when partially applied.
add5 :: Int -> Int
add5 x = curriedAdd 5 x
-- add5 = curriedAdd 5

-- composing two functions
add10 :: Int -> Int
add10 = add5 . add5

spec :: Spec
spec = do

    describe "curried vs un-curried functions" $ do
        it "should add two literals using a curried function" $ do
            curriedAdd 10 5 `shouldBe` 15
        it "should add two literals using a un-curried function" $ do
            uncurriedAdd(10, 5) `shouldBe` 15

    describe "partial application" $ do
        it "should add 5 to a given literal" $ do
            add5 5 `shouldBe` 10
        it "should add 10 to a given literal" $ do
            add10 10 `shouldBe` 20