module Functions where

import Test.Hspec

allEqual :: (Eq a, Num a) => a -> a -> a -> Bool
allEqual x y z = x == y && y == z

spec :: Spec
spec = do

    describe "allEqual" $ do
        it "should compare tree numbers for equality" $ do
            allEqual 2 2 2 `shouldBe` True