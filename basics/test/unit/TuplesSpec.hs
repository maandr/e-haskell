module TuplesSpec where

import Test.Hspec
import Data.Tuple --swap

spec :: Spec
spec = do

    describe "Comparison" $ do
        it "Should compare equality on behalf of the tuples first component, then on second component" $ do
            (1, 5) > (2, 1) `shouldBe` False
            (2, 0) > (1, 8) `shouldBe` True
            (2, 0) >= (2, 5) `shouldBe` False
            (2, 0) == (2, 5) `shouldBe` False
            (1, 5) == (1, 5) `shouldBe` True
            (2, 0) < (2, 5) `shouldBe` True
            (2, 0) <= (2, 0) `shouldBe` True

    describe "fst" $ do
        it "Should return first component of a tuple" $ do
            fst (9, 4) `shouldBe` 9
    
    describe "snd" $ do
        it "Should return second component of a tuple" $ do
            snd (9, 4) `shouldBe` 4

    describe "swap" $ do
        it "Should swap the two components of a tuple" $ do
            swap (9, 4) `shouldBe` (4, 9)