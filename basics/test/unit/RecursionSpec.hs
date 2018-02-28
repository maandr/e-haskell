module RecursionSpec where

import Test.Hspec

-- calculate faculty of given integer recursivly
fac :: Integer -> Integer
fac 0 = 0
fac 1 = 1
fac x = x + fac (x - 1)

spec :: Spec
spec = do

    describe "Recursion" $ do
        it "should calculate faculty of given integer" $ do
            fac 3 `shouldBe` 6
