{-# LANGUAGE RecordWildCards #-}

module FunctorSpec where

import Test.Hspec

-- typeclass IsEmpty
-- enforeces types to provide a isEmpty function
class IsEmpty a where
    isEmpty :: a -> Bool

-- datatype Optional
data Optional a = Null | Optional {
    value :: a
} deriving (Eq, Show)

-- have Optional implement typeclass Functor (fmap)
instance Functor Optional where
    fmap f Optional{..} = Optional {
        value = f value
    }
    fmap _ Null = Null

-- have Optional implement typeclass IsEmpty (isEmpty)
instance IsEmpty (Optional a) where
    isEmpty Null = True
    isEmpty _ = False

empty :: Optional a
empty = Null

spec :: Spec
spec = do

    describe "fmap" $ do
        it "should apply a function on value helt in a optional" $ do
            fmap (\x -> x * x) (Optional 10) `shouldBe` (Optional 100)
            fmap (\x -> x ++ x) (Optional "foo") `shouldBe` (Optional "foofoo")
            fmap (\x -> x ++ x) (Optional [1, 2]) `shouldBe` (Optional [1, 2, 1, 2])
        it "should return a empty optional when apply a function on a empty optional" $ do
            fmap (\x -> x * x) Null `shouldBe` Null
    describe "isEmpty" $ do
        it "should return False for a optional holding a value" $ do
            isEmpty (Optional 10) `shouldBe` False
            isEmpty (Optional "John") `shouldBe` False
            isEmpty (Optional [1.5, 3.3, 4.3]) `shouldBe` False
        it "should return True for a empty optional" $ do
            isEmpty Null `shouldBe` True


