module ListsSpec where
 
import Lists
import Test.Hspec

spec :: Spec
spec = do

    describe "(!!)" $ do
        it "should return the element at a given position from the list" $ do
            [1, 2, 3] !! 1 `shouldBe` 2
            "Haskell" !! 0 `shouldBe` 'H'
        it "should return the element at a given position from an infinit list" $ do
            [0..] !! 999 `shouldBe` 999
            [1..] !! 999 `shouldBe` 1000

    describe "(:)" $ do
        it "should prepend literal to a list" $ do
            3 : [1, 2] `shouldBe` [3, 1, 2]
        it "should prepend literal to a empty list" $ do
            7 : [] `shouldBe` [7]
        it "should prepend literal to a string"  $ do
            'A' : " Ton Of Love" `shouldBe` "A Ton Of Love"
        it "should prepend tuple to list of tuples" $ do
            ("k1", "v1") : [("k0", "v0")] `shouldBe` [("k1", "v1"), ("k0", "v0")]

    describe "(++)" $ do
        it "should append list to another list" $ do
            [1, 2] ++ [3, 4] `shouldBe` [1, 2, 3, 4]
        it "should concatinate two strings" $ do
            "Hello" ++ " World" `shouldBe` "Hello World"
        it "should concatinate a list of two tuples" $ do
            [("k1", "v1")] ++ [("k0", "v0")] `shouldBe` [("k1", "v1"), ("k0", "v0")]
            [("k0", "v0")] ++ [("k1", "v1")] `shouldBe` [("k0", "v0"), ("k1", "v1")]

    describe "length" $ do
        it "should return the length of a list" $ do
            length [1, 2, 3] `shouldBe` 3
            length "Hello" `shouldBe` 5
        it "should return the length of a empty list" $ do
            length [] `shouldBe` 0

    describe "elem" $ do
        it "should return True if a element is in a list" $ do
            elem 3 [1, 2, 3, 4] `shouldBe` True
            elem 'p' "Hodgepodge" `shouldBe` True
        it "should return False if a element is not in a list" $ do
            elem 5 [1, 2, 3, 4] `shouldBe` False
            elem 's' "Hodegpodge" `shouldBe` False

    describe "head" $ do
        it "should return the head of a list" $ do
            head [1, 2, 3] `shouldBe` 1
            head "Hello" `shouldBe` 'H'
        it "should return the head of a infinit list" $ do
            head [3, 6 ..] `shouldBe` 3
    
    describe "tail" $ do
        it "should return the tail of a list" $ do
            tail [1, 2, 3] `shouldBe` [2, 3]
        it "should return the tail of a string" $ do
            tail "Hello" `shouldBe` "ello"
    
    describe "take" $ do
        it "should take given amount of literals from a list" $ do
            take 1 [1, 2, 3] `shouldBe` [1]
            take 2 [1, 2, 3] `shouldBe` [1, 2]
            take 3 ['a'..'z'] `shouldBe` ['a', 'b', 'c']
            take 3 "Hello" `shouldBe` ['H', 'e', 'l']
        it "should take given amount of literals from a infinit list" $ do
            take 3 [2, 4..] `shouldBe` [2, 4, 6]
        it "should return empty list when applied to empty list" $ do
            (take 1 [] :: [Int]) `shouldBe` []
    
    describe "repeat" $ do
        it "should take a literal and repeat it into an infinit list" $ do
            (take 5 $ repeat 5) `shouldBe` [5, 5, 5, 5 ,5]
            (take 3 $ repeat "na") `shouldBe` ["na", "na", "na"]
    
    describe "cycle" $ do
        it "should take a list and repeat it into an infinit list" $ do
            (take 6 $ cycle [1, 2]) `shouldBe` [1, 2, 1, 2, 1, 2]
            (take 7 $ cycle "dumdi") `shouldBe` "dumdidu"

    describe "zip" $ do
        it "should take a list and combine it with another list" $ do
            zip [1, 2, 3] ['a', 'b', 'c'] `shouldBe` [(1, 'a'), (2, 'b'), (3, 'c')]
        it "should should stop if any of the two lists is exhausted" $ do
            zip [1, 2, 3] ['a'..'z'] `shouldBe` [(1, 'a'), (2, 'b'), (3, 'c')]
            zip [1..] ['a'..'c'] `shouldBe` [(1, 'a'), (2, 'b'), (3, 'c')]

    describe "concat" $ do
        it "should concatinate a list of lists" $ do
            concat [[1, 2, 3], [4, 5]] `shouldBe` [1, 2, 3, 4, 5]
        it "should concatinate a list of list including and empty list" $ do
            concat [[1, 2, 3], []] `shouldBe` [1, 2, 3]
        it "should concatinate a list of strings" $ do
            concat ["na", "na", "na"] `shouldBe` "nanana"
        it "should concatinate two empty lists" $ do
            concat [[] :: [Int], [] :: [Int]] `shouldBe` []

    describe "map" $ do
        it "should apply a function to each element of a list" $ do
            map (\x -> x * x) [1, 2, 3] `shouldBe` [1, 4, 9]
            map (\x -> x + x) [5, 10, 20] `shouldBe` [10, 20, 40]
            map isEvan [0..5] `shouldBe` [True, False, True, False, True, False]

    describe "fmap" $ do
        it "should apply a function to each element of a list" $ do
            fmap (\x -> x + x) [1, 2, 3] `shouldBe` [2, 4, 6]
            fmap (\x -> x + 10) [5, 10, 20] `shouldBe` [15, 20, 30]
            fmap isEvan [0..5] `shouldBe` [True, False, True, False, True, False]

    describe "isEvan" $ do
        it "should return True given an evan number" $ do
            isEvan 2 `shouldBe` True
        it "should return False given an odd number" $ do
            isEvan 3 `shouldBe` False

    describe "getEvanNumbersUntilMonad" $ do
        it "should return list of evan numbers in the range 0..10" $ do
            getEvanNumbersUntilMonad 10 `shouldBe` [0, 2, 4, 6, 8, 10]
    
    describe "getEvanNumbersUntilListComprehention" $ do
        it "should return list of evan numbers in the range 0..10" $ do
            getEvanNumbersUntilListComprehention 10 `shouldBe` [0, 2, 4, 6, 8, 10]
