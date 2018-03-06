{-# LANGUAGE OverloadedStrings #-}

module AesonParserSpec where

import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Data.Text
import Data.Scientific ( toBoundedInteger )
import Data.Aeson ( decode )
import Data.Aeson.Types (
          Parser
        , Value(..)
        , toJSON
        , parseEither
    )
import GHC.Exts ( fromList ) 
import Test.Hspec

parseTuple :: Value -> Parser (String, Int)
parseTuple (Object object) = do
    let maybeNameField = HashMap.lookup "name" object
        maybeAgeField = HashMap.lookup "age" object

    nameField <- case maybeNameField of
        Just x -> return x
        Nothing -> fail "Field 'name' does not exist"
    
    name <- case nameField of
        String x -> return (unpack x)
        _ -> fail "Field 'name' must be a string"
    
    ageField <- case maybeAgeField of
        Just x -> return x
        Nothing -> fail "Field 'age' does not exist"

    maybeAge <- case ageField of
        Number x -> return $ toBoundedInteger x
        _ -> fail "Field 'age' must be a number"
    
    age <- case maybeAge of
        Just x -> return x
        Nothing -> fail "Field 'age' has to be a integer"

    return (name, age)

parseTuple val = fail $ "exptected an object but got: " ++ show val

spec :: Spec
spec = do

    describe "parseTuple" $ do
        it "should parse json to a tuple" $ do
            -- given
            let jsonString = "{\"name\":\"John\",\"age\":45}"
            let Just json = decode jsonString :: Maybe Value

            -- when + then
            (parseEither parseTuple json) `shouldBe` Right ("John", 45)