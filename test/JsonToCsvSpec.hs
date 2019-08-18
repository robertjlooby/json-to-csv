{-# LANGUAGE OverloadedStrings #-}

module JsonToCsvSpec
  ( main
  , spec
  ) where

import           JsonToCsv
import           Test.Hspec
import           Test.QuickCheck

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "convert" $ do
        it "returns an empty csv for and empty object" $ do
            convert "{}" `shouldBe` Right "\r\n\r\n"

        it "returns a single row for a simple object with a boolean value" $ do
            convert "{\"one\": \"first\", \"two\": true}"
                `shouldBe` Right "two,one\r\nTrue,first\r\n"

        it "returns a single row for a simple object with a null value" $ do
            convert "{\"one\": \"first\", \"two\": null}"
                `shouldBe` Right "two,one\r\n,first\r\n"

        it "returns a single row for a simple object with a number value" $ do
            convert "{\"one\": \"first\", \"two\": 123.45}"
                `shouldBe` Right "two,one\r\n123.45,first\r\n"

        it "returns a single row for a simple object with a string value" $ do
            convert "{\"one\": \"first\", \"two\": \"second\"}"
                `shouldBe` Right "two,one\r\nsecond,first\r\n"
