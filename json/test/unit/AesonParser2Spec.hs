{-# LANGUAGE OverloadedStrings #-}

module AesonParser2Spec where

import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Data.Text
import Data.Scientific ( toBoundedInteger )
import Data.Aeson (
          decode
        , withArray
        , withObject
        , parseJSON
    )
import Data.Aeson.Types (
          Parser
        , Value(..)
        , toJSON
        , parseEither
    )
import GHC.Exts ( fromList ) 
import Test.Hspec

parseTuples :: Value -> Parser [(String, Int)]
parseTuples = withArray "array of tuples" $ \array -> mapM parseTuple (Vector.toList array)

parseTuple :: Value -> Parser (String, Int)
parseTuple = withObject "tuple" $ \object -> do
    -- parse name
    name <- case HashMap.lookup "name" object of
        Just x -> parseJSON x
        Nothing -> fail $ fieldDoesNotExist "name"

    -- parse age
    age <- case HashMap.lookup "age" object of
        Just x -> parseJSON x
        Nothing -> fail $ fieldDoesNotExist "age"
    
    return (name, age)

fieldDoesNotExist :: String -> String
fieldDoesNotExist fieldName = "Field '" ++ fieldName ++ "' does not exist"

spec :: Spec
spec = do

    describe "parseTuple" $ do
        it "should parse json to a tuple" $ do
            -- given
            let jsonString = "{\"name\":\"John\",\"age\":45}"
            let Just json = decode jsonString :: Maybe Value

            -- when + then
            (parseEither parseTuple json) `shouldBe` Right ("John", 45)
    
    describe "parseTuples" $ do
        it "should parse json to a list of tuples" $ do
            -- given
            let jsonString = "[{\"name\":\"John\",\"age\":45},{\"name\":\"Mary\",\"age\":32}]"
            let Just json = decode jsonString :: Maybe Value

            -- when + then
            (parseEither parseTuples json) `shouldBe` Right [("John", 45), ("Mary", 32)]