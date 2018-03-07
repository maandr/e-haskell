{-# LANGUAGE RecordWildCards #-}

module FunctorSpec where

import Test.Hspec

-- typeclass is empty
class IsEmpty a where
    isEmpty :: a -> Bool

-- datatype Optional
data Optional a = Null | Optional {
    value :: a
} deriving (Eq, Show)

-- have Optional implement typeclass Functor
instance Functor Optional where
    fmap f Optional{..} = Optional {
        value = f value
    }
    fmap _ Null = Null

-- have Optional implement typeclass IsEmpty
instance IsEmpty (Optional a) where
    isEmpty Null = True
    isEmpty _ = False

spec :: Spec
spec = do

    describe "Optional Functor" $ do
        it "should apply a function using fmap on the Optional functor" $ do
            -- given
            let optional = Optional 10 :: Optional Int

            -- when + then
            (fmap (\x -> x * x) optional) `shouldBe` (Optional 100)
            isEmpty optional `shouldBe` False
        it "should apply a function using fmap on a empty Optional functor" $ do
            -- given
            let optional = Null :: Optional Int

            (fmap (\x -> x * x) optional) `shouldBe` Null
            isEmpty optional `shouldBe` True


