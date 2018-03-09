{-# LANGUAGE RecordWildCards #-}

module FunctorSpec where

import Test.Hspec

-- typeclass IsEmpty
-- enforces types to provide a isEmpty function
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

data Animal = 
      Cat String Int --name, lives
    | Dog String Bool --name, isAlive
    deriving (Eq, Show)

class Nameable a where
    getName :: a -> String

instance Nameable Animal where
    getName (Cat name _) = name
    getName (Dog name _) = name

isAlive :: Animal -> Bool
isAlive (Cat _ lives) = lives > 0
isAlive (Dog _ isAlive) = isAlive

spec :: Spec
spec = do

    describe "getName" $ do
        it "should get the name of an animal" $ do
            getName (Cat "Snickers" 7) `shouldBe` "Snickers"
            getName (Dog "Rex" True) `shouldBe` "Rex"

    describe "isAlive" $ do
        it "should determine wether or not a animal is alive" $ do
            isAlive (Cat "Snickers" 7) `shouldBe` True
            isAlive (Dog "Rex" True) `shouldBe` True
            isAlive (Cat "Whiskers" 0) `shouldBe` False
            isAlive (Dog "Snoopy" False) `shouldBe` False

    describe "fmap" $ do
        it "should apply a function on value helt in a optional" $ do
            fmap (\x -> x * x) (Optional 10) `shouldBe` (Optional 100)
            fmap (\x -> x ++ x) (Optional "foo") `shouldBe` (Optional "foofoo")
            fmap (\x -> x ++ x) (Optional [1, 2]) `shouldBe` (Optional [1, 2, 1, 2])
            fmap isAlive (Optional $ Cat "Snickers" 7) `shouldBe` (Optional True)
            {-- TODO: figure out how to fmap over it
            fmap isAlive (Optional [
                      Cat "Snickers" 7
                    , Dog "Rex" True
                    , Cat "Whiskers" 0
                    , Dog "Snoopy" False
                ]) `shouldBe` (Optional [True, True, False, False])
            --}

        it "should return a empty optional when apply a function on a empty optional" $ do
            fmap (\x -> x * x) Null `shouldBe` Null

    describe "isEmpty" $ do
        it "should return False for a optional holding a value" $ do
            isEmpty (Optional 10) `shouldBe` False
            isEmpty (Optional "John") `shouldBe` False
            isEmpty (Optional [1.5, 3.3, 4.3]) `shouldBe` False

        it "should return True for a empty optional" $ do
            isEmpty Null `shouldBe` True


