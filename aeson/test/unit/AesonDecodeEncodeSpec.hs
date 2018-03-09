{-# LANGUAGE OverloadedStrings #-}

module AesonDecodeEncodeSpec where

import Data.Text
import Data.Aeson
import Data.Aeson.Types ( 
         object
        , Value(..)
    )
import Test.Hspec
import GHC.Exts ( fromList )

spec :: Spec
spec = do

    -- given
    let integers = [1, 2, 3] :: [Int]
    let floats = [1, 2, 3] :: [Float]
    let bools = [True, False, False] :: [Bool]
    let strings = ["green", "blue", "red"] :: [String]

    let book :: Value
        book = Object $ fromList [
                ("pages", Number 99),
                ("author", String "Franz Kafka")
            ]

    let movie :: Value
        movie = object [
                "title" .= String "Star Wars",
                "genres" .= [String "sci-fi", String "adventure"],
                "year" .= Number 1977
            ]

    describe "encode" $ do
        it "should encode literals to json" $ do
            encode (99 :: Int) `shouldBe` "99"
            encode (3.14 :: Float) `shouldBe` "3.14"
            encode False `shouldBe` "false"
            encode ("green" :: Text)  `shouldBe` "\"green\""

        it "should encode a list of literals to json" $ do
            encode integers `shouldBe` "[1,2,3]"
            encode floats `shouldBe` "[1.0,2.0,3.0]"
            encode bools `shouldBe` "[true,false,false]"
            encode strings  `shouldBe` "[\"green\",\"blue\",\"red\"]"

        it "should encode complex types to json" $ do
            encode book `shouldBe` "{\"pages\":99,\"author\":\"Franz Kafka\"}"
            encode movie `shouldBe` "{\"year\":1977,\"genres\":[\"sci-fi\",\"adventure\"],\"title\":\"Star Wars\"}"
    
    describe "decode" $ do
        it "should decode json to literals" $ do
            (decode "99" :: Maybe Int) `shouldBe` Just 99
            (decode "3.14" :: Maybe Float) `shouldBe` Just 3.14
            (decode "false" :: Maybe Bool) `shouldBe` Just False
            (decode "\"green\"" :: Maybe String) `shouldBe` Just "green"

        it "should decode json to a list of literals" $ do
            (decode "[1,2,3]" :: Maybe [Int]) `shouldBe` Just [1, 2, 3]
            (decode "maleformed" :: Maybe [Int]) `shouldBe` Nothing