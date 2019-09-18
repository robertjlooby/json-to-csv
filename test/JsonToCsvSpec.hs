{-# LANGUAGE OverloadedStrings #-}

module JsonToCsvSpec
  ( main
  , spec
  ) where

import           JsonToCsv
import           Test.Hspec

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "convert" $ do
        it "returns an empty csv for and empty object" $ do
            convert "{}" `shouldBe` Right "\r\n\r\n"

        it "handles a bare boolean" $ do
            convert "true" `shouldBe` Right "TRUE\r\n"

        it "handles a bare null" $ do
            convert "null" `shouldBe` Right "\r\n"

        it "handles a bare number" $ do
            convert "123456789" `shouldBe` Right "123456789\r\n"

        it "handles a bare string" $ do
            convert "\"test\"" `shouldBe` Right "test\r\n"

        it "returns a single row for a simple object with a boolean value" $ do
            convert "{\"one\": \"first\", \"two\": true}"
                `shouldBe` Right "one,two\r\nfirst,TRUE\r\n"

        it "returns a single row for a simple object with a null value" $ do
            convert "{\"one\": \"first\", \"two\": null}"
                `shouldBe` Right "one,two\r\nfirst,\r\n"

        it "returns a single row for a simple object with a number value" $ do
            convert "{\"one\": \"first\", \"two\": 1.23456789}"
                `shouldBe` Right "one,two\r\nfirst,1.23456789\r\n"

        it "returns a single row for a simple object with a string value" $ do
            convert "{\"one\": \"first\", \"two\": \"second\"}"
                `shouldBe` Right "one,two\r\nfirst,second\r\n"

        it "returns multiple rows for an array of objects" $ do
            convert
                "[{\"one\": \"first\"}, {\"one\": \"second\"}, {\"one\": \"third\"}]"
                `shouldBe` Right "one\r\nfirst\r\nsecond\r\nthird\r\n"

        it "handles arrays of objects of varying shapes" $ do
            convert
                "[{\"one\": 1, \"two\": 2, \"arr\": [0]}, {\"one\": 11, \"three\": 33}, {\"one\": 111, \"arr\": [0, 1, 2]}]"
                `shouldBe` Right
                    "arr[0],arr[1],arr[2],one,three,two\r\n0,,,1,,2\r\n,,,11,33,\r\n0,1,2,111,,\r\n"

        it "handles arrays of objects and other top level values" $ do
            convert "[123, {\"one\": 1, \"two\": 2}, \"value\"]"
                `shouldBe` Right "one,two,[0],[2]\r\n1,2,123,value\r\n"

        it "handles an array as an object value" $ do
            convert
                "{\"top\": \"level\", \"inner\": [{\"item\": \"first\", \"thing\": \"1\"}, {\"item\": \"second\", \"thing\": \"2\"}]}"
                `shouldBe` Right
                    "inner.item,inner.thing,top\r\nfirst,1,level\r\nsecond,2,level\r\n"

        it "handles an array with non-object values as an object value" $ do
            convert
                "{\"top\": \"level\", \"inner\": [\"first\", \"second\", 1, [\"double\", \"nested\"], false, null]}"
                `shouldBe` Right
                    "inner[0],inner[1],inner[2],inner[3][0],inner[3][1],inner[4],inner[5],top\r\nfirst,second,1,double,nested,FALSE,,level\r\n"

        it "handles an object as an object value" $ do
            convert
                "{\"top\": {\"inner\": \"thing\"}, \"other\": [{\"item\": \"first\"}, {\"item\": \"second\"}]}"
                `shouldBe` Right
                    "other.item,top.inner\r\nfirst,thing\r\nsecond,thing\r\n"
