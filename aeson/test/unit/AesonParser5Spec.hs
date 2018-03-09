{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AesonParser5Spec where

import Test.Hspec
import Data.Aeson.Types (
          Value(..)
        , parseEither
    )
import Data.Aeson (
          FromJSON
        , ToJSON
        , parseJSON
        , toJSON
        , (.=)
        , (.:)
        , (.:?)
        , (.!=)
        , object
        , withObject
        , encode
        , decode
    )

data Person = Person {
        name :: String,
        age :: Int
    }
    deriving (Show, Eq)

instance FromJSON Person where
    parseJSON = withObject "person" $ \o -> do
        firstName <- o .: "firstName"
        lastName <- o .:? "lastName" .!= "Doe" --optional defaults to "Doe"
        let name = firstName ++ " " ++ lastName
        age <- o .:? "age" .!= 30 --optional defaults to 30
        return Person{..}

instance ToJSON Person where
    toJSON Person{..} = object [
              "name" .= name
            , "age" .= age
        ]

spec :: Spec
spec = do

    describe "ToJSON" $ do
        it "should serialize a person to JSON" $ do
            -- given
            let person = Person "John" 45

            -- when + then
            (encode $ toJSON person) `shouldBe` "{\"age\":45,\"name\":\"John\"}"

    describe "FromJSON" $ do
        it "should deserialize JSON to a person" $ do
            -- given
            let jsonString = "{\"age\":45,\"firstName\":\"John\",\"lastName\":\"Goodman\"}"
            let Just json = decode jsonString :: Maybe Value

            -- when + then
            (parseEither parseJSON json) `shouldBe` Right (Person "John Goodman" 45)
        it "should deserialize JSON missing a optional field" $ do
            -- given
            let jsonString = "{\"firstName\":\"John\"}"
            let Just json = decode jsonString :: Maybe Value

            -- when + then
            (parseEither parseJSON json) `shouldBe` Right (Person "John Doe" 30)
        it "should fail to deserialize JSON missing required field" $ do
            -- given
            let Just json = decode "{\"lastName\":\"Goodman\"}" :: Maybe Value

            -- when + then
            (parseEither parseJSON json :: Either String Person) `shouldBe` Left "Error in $: key \"firstName\" not present"