module DictionarySpec where

import Test.Hspec

data Dict k v = Dict [(k, v)] deriving (Eq, Show)

dictAddToHead :: Dict k v -> k -> v -> Dict k v
dictAddToHead (Dict d) k v = Dict ((k, v):d)

dictAddToTail :: Dict k v -> k -> v -> Dict k v
dictAddToTail (Dict d) k v = Dict (d ++ [(k, v)])

dictFind :: Eq k => Dict k v -> k -> Maybe v
dictFind (Dict []) _ = Nothing
dictFind (Dict ((key, value):dict)) search =
    if key == search
    then Just value
    else dictFind (Dict dict) key

dictMap :: (a -> b) -> Dict k a -> Dict k b
dictMap _ (Dict []) = Dict []
dictMap func (Dict ((key, a'):dict)) =
    let b' = func a'
        Dict dict' = dictMap func (Dict dict)
    in Dict ((key, b'):dict')

spec :: Spec
spec = do

    describe "dictAddToHead" $ do
        it "should add an item to the head of a empty dictionary" $ do
            -- given
            let d = Dict []

            -- when + then
            dictAddToHead d "foo" "bar" `shouldBe` Dict[("foo", "bar")]
        it "should add an item to the head of a dictionary" $ do
            -- given
            let d = Dict [("k1", "v1")]

            -- when + then
            dictAddToHead d "k2" "v2" `shouldBe` Dict[("k2", "v2"), ("k1", "v1")]
    
    describe "dictAddToTail" $ do
        it "should add an item to the tail of a empty dictionary" $ do
            -- given
            let d = Dict []

            -- when + then
            dictAddToTail d "k1" "v1" `shouldBe` Dict[("k1", "v1")]
        it "should add an item to the tail of a dictironary" $ do
            -- given
            let d = Dict [("k1", "v1")]

            -- when + then
            dictAddToTail d "k2" "v2" `shouldBe` Dict[("k1", "v1"), ("k2", "v2")]

    describe "dictMap" $ do
        it "should return a empty dictionary when given a empty dictionary" $ do
            -- given
            let d = Dict [] :: Dict String Int
                f x = x * x

            -- when + then
            dictMap f d `shouldBe` Dict []
        it "should apply the given function to each value within the given dictionary" $ do
            -- given
            let d = Dict [("one", 1)
                         ,("two", 2)
                         ,("three", 3)
                         ]
                f x = x * x

            -- when + then
            dictMap f d `shouldBe` Dict[("one", 1), ("two", 4), ("three", 9)]

