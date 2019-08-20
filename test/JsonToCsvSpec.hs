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
            convert "123.45" `shouldBe` Right "123.45\r\n"

        it "handles a bare string" $ do
            convert "\"test\"" `shouldBe` Right "test\r\n"

        it "returns a single row for a simple object with a boolean value" $ do
            convert "{\"one\": \"first\", \"two\": true}"
                `shouldBe` Right "two,one\r\nTRUE,first\r\n"

        it "returns a single row for a simple object with a null value" $ do
            convert "{\"one\": \"first\", \"two\": null}"
                `shouldBe` Right "two,one\r\n,first\r\n"

        it "returns a single row for a simple object with a number value" $ do
            convert "{\"one\": \"first\", \"two\": 123.45}"
                `shouldBe` Right "two,one\r\n123.45,first\r\n"

        it "returns a single row for a simple object with a string value" $ do
            convert "{\"one\": \"first\", \"two\": \"second\"}"
                `shouldBe` Right "two,one\r\nsecond,first\r\n"

        it "returns multiple rows for an array of objects" $ do
            convert
                "[{\"one\": \"first\"}, {\"one\": \"second\"}, {\"one\": \"third\"}]"
                `shouldBe` Right "one\r\nfirst\r\nsecond\r\nthird\r\n"

        it "handles arrays of objects of varying shapes" $ do
            convert
                "[{\"one\": 1, \"two\": 2}, {\"one\": 11, \"three\": 33}, {\"one\": 111}]"
                `shouldBe` Right
                    "two,one,three\r\n2.0,1.0,\r\n,11.0,33.0\r\n,111.0,\r\n"

        it "handles an array as an object value" $ do
            convert
                "{\"top\": \"level\", \"inner\": [{\"item\": \"first\", \"thing\": \"1\"}, {\"item\": \"second\", \"thing\": \"2\"}]}"
                `shouldBe` Right
                    "inner->item,top,inner->thing\r\nfirst,level,1\r\nsecond,level,2\r\n"

        it "handles an object as an object value" $ do
            convert
                "{\"top\": {\"inner\": \"thing\"}, \"other\": [{\"item\": \"first\"}, {\"item\": \"second\"}]}"
                `shouldBe` Right
                    "top->inner,other->item\r\nthing,first\r\nthing,second\r\n"
