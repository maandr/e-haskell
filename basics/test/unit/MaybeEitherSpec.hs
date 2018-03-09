module MaybeEitherSpec where

import Test.Hspec

safeDiv :: Float -> Float -> Either String Float
safeDiv x 0 = Left "division by zero not allowed."
safeDiv x y = Right (x / y)

spec :: Spec
spec = do

    describe "Maybe" $ do
        it "should unpack Maybe" $ do
            -- given
            let x = Just 10 :: Maybe Int
            let Just y = Just 20 :: Maybe Int

            -- when + then
            x `shouldBe` Just 10
            y `shouldBe` 20

    describe "Either" $ do
        it "should unpack right value of Either" $ do
            -- given
            let x = Right 10 :: Either String Int
            let Right y = Right 10 :: Either String Int

            -- when + then
            x `shouldBe` Right 10
            y `shouldBe` 10
        it "should unpack left value of Either" $ do
            -- given
            let x = Left "fail" :: Either String Int
            let Left y = Left "fail" :: Either String Int

            -- when + then
            x `shouldBe` Left "fail"
            y `shouldBe` "fail"

    
    describe "safeDiv" $ do
        it "should divide two floats" $ do
            safeDiv 11.0 0.0 `shouldBe` Left "division by zero not allowed."
            safeDiv 10.0 5.0 `shouldBe` Right 2.0
            safeDiv 9.0 3.0 `shouldBe` Right 3.0
            