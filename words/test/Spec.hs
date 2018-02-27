import Data
import Lib
import Test.Hspec

main :: IO ()
main = hspec $ do

    let givenEmpty = [] :: Grid
    let givenOne = ["ax"] :: Grid
    let givenTwo = [ "ax", "by" ] :: Grid
    let givenThree = [ "ax", "by", "cz" ] :: Grid

    describe "formatGrid" $ do
        it "Should concatenate every line with a newline" $ do
            formatGrid givenTwo `shouldBe` "ax\nby\n"
        it "Should hanlde list containing one element" $ do
            formatGrid givenOne `shouldBe` "ax\n"
        it "Should handle empty list" $ do
            formatGrid givenEmpty `shouldBe` ""
    
    describe "getPossibilities" $ do
        it "Should get all possibilities of the Grid" $ do
            getPossibilities givenTwo `shouldBe` [ "ax", "by", "ab", "xy", "a_", "xb", "y", "x_", "ay", "b", "xa", "yb", "ba", "yx", "_a", "bx", "y", "_x", "ya", "b" ]
        it "Should handle list containing one element" $ do
            getPossibilities givenOne `shouldBe` [ "ax", "a", "x", "a", "x", "x", "a", "xa", "a", "x", "a", "x", "x", "a" ]
        it "Should handle empty list" $ do
            getPossibilities givenEmpty `shouldBe` []
    
    describe "skew" $ do
        it "Should offset each element of the list by one off to it's predecessor" $ do
            skew givenThree `shouldBe` [ "ax", "_by", "__cz" ]
        it "Should handle list containing one element" $ do
            skew givenOne `shouldBe` [ "ax" ]
        it "Should handle empty list" $ do
            skew givenEmpty `shouldBe` []

    describe "findWordInLine" $ do
        it "Should find word that exists in a line" $ do
            findWordInLine "HASKELL" "__A_HASKELL__BA_E" `shouldBe` True
        it "Should not find word that does not exist in a line" $ do
            findWordInLine "RUBY" "__A_HASKELL__BA_E" `shouldBe` False

    describe "findWord" $ do
        it "Should find words that exist on the Grid" $ do
            findWord grid "HASKELL" `shouldBe` Just "HASKELL"
            findWord grid "PHP" `shouldBe` Just "PHP"
            findWord grid "CSHARP" `shouldBe` Just "CSHARP"
        it "Should not find words that do not exist on the Grid" $ do
            findWord grid "JAVA" `shouldBe` Nothing

    describe "findWords" $ do
        it "Should find all the words that exist on the Grid" $ do
            findWords grid languages `shouldBe` languages