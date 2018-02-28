import Lists
import Test.Hspec

main :: IO ()
main = hspec $ do

    describe "isEvan" $ do
        it "Should return True given an evan number" $ do
            isEvan 2 `shouldBe` True
        it "Should return False given an odd number" $ do
            isEvan 3 `shouldBe` False

    describe "getEvanNumbersUntilMonad" $ do
        it "Should return list of evan numbers in the range 0..10" $ do
            getEvanNumbersUntilMonad 10 `shouldBe` [0, 2, 4, 6, 8, 10]
    
    describe "getEvanNumbersUntilListComprehention" $ do
        it "Should return list of evan numbers in the range 0..10" $ do
            getEvanNumbersUntilListComprehention 10 `shouldBe` [0, 2, 4, 6, 8, 10]
