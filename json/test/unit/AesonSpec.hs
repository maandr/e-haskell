{-# LANGUAGE OverloadedStrings #-}

module AesonSpec where

import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector
import Data.Text
import Data.Aeson
import Data.Aeson.Types
import Test.Hspec
import GHC.Exts

spec :: Spec
spec = do

    -- given
    let integers = [1, 2, 3] :: [Int]
    let floats = [1, 2, 3] :: [Float]
    let bools = [True, False, False] :: [Bool]
    let strings = ["green", "blue", "red"] :: [String]

    -- with GHC.Exts fromList
    let book :: Value
        book = Object $ fromList [
                ("pages", Number 99),
                ("author", String "Franz Kafka")
            ]

    -- with object function
    let movie :: Value
        movie = object [
                "title" .= String "Star Wars",
                "genres" .= [String "sci-fi", String "adventure"],
                "year" .= Number 1977
            ]
    --{--
    let parseTuple :: Value -> Parser (String, Bool)
        parseTuple (Object o) = do
            -- lookup the "a" field
            let mbFieldA = HashMap.lookup "a" o

            -- fail if it wasn't found
            fieldA <- case mbFieldA of
                Just x -> return x
                Nothing -> fail "no field 'a'"
            
            -- extract the value from it, or fail if it's of the wrong type
            a <- case fieldA of
                String x -> return (T.unpack x)
                _ -> fail "expected a string"
                
            -- do all the same for b (in a slightly terser way)
            b <- case HashMap.lookup "b" o of
                Just (Bool x) -> return x
                Just _ -> fail "expected a boolean"
                Nothing -> fail "no field 'b'"

            return (a, b)
        parseTuple _ = fail "expected an object"
    --}

    let parseArray :: Value -> Parser [(String, Bool)]
        parseArray (Array a) = mapM parseTuple (Vector.toList a)
        parseArray _ = fail "expected an array"

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
        it "should decode json to complex types" $ do
            (parseMaybe parseTuple "{\"a\":\"foo\",\"b\":true}" :: Maybe (String, Bool)) `shouldBe` Just ("foo", True)