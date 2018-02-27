import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Test findWords function" $ do
        it "Should find all languages contained by the grid" $ do
            and (map (findWord grid) languages) `shouldBe` True
