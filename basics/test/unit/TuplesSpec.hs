module TuplesSpec where

import Test.Hspec
import Data.Tuple --swap

spec :: Spec
spec = do

    describe "fst" $ do
        it "Should return first component of a tuple" $ do
            fst (9, 4) `shouldBe` 9
    
    describe "snd" $ do
        it "Should return second component of a tuple" $ do
            snd (9, 4) `shouldBe` 4

    describe "swap" $ do
        it "Should swap the two components of a tuple" $ do
            swap (9, 4) `shouldBe` (4, 9)