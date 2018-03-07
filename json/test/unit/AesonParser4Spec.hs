{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AesonParser4Spec where

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
        , object
        , withObject
        , encode
        , decode
    )

data Person = Person {name :: String, age :: Int}
    deriving (Show, Eq)

instance FromJSON Person where
    parseJSON = withObject "person" $ \o -> do
        name <- o .: "name"
        age <- o .: "age"
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
            let jsonString = ("{\"age\":45,\"name\":\"John\"}")
            let Just json = (decode jsonString) :: Maybe Value

            -- when + then
            (parseEither parseJSON json) `shouldBe` Right (Person "John" 45)
            