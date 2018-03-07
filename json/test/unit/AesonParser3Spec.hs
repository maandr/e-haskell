{-# LANGUAGE OverloadedStrings #-}

module AesonParser3Spec where

import Test.Hspec
import Data.Aeson (
          FromJSON
        , ToJSON
        , parseJSON
        , toJSON
        , (.=)
        , (.:)
        , object
        , withObject
        , encode
    )

data Person = Person {name :: String, age :: Int}

instance FromJSON Person where
    parseJSON = withObject "person" $ \o ->
        Person <$> o .: "name" <*> o.: "age"

instance ToJSON Person where
    toJSON p = object [
            "name" .= name p
            , "age" .= age p
        ]

spec :: Spec
spec = do

    describe "ToJSON" $ do
        it "should serialize a person to JSON" $ do
            -- given
            let person = Person "John" 45

            -- when + then
            (encode $ toJSON person) `shouldBe` "{\"age\":45,\"name\":\"John\"}"
            