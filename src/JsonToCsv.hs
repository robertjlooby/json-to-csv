{-# LANGUAGE OverloadedStrings #-}

module JsonToCsv
  ( convert
  ) where

import           Data.Aeson ( Object, Value(..), eitherDecode )
import qualified Data.ByteString.Lazy as B
import           Data.Csv ( encodeByName, header )
import           Data.HashMap.Strict as HM
import           Data.HashSet as HS
import           Data.Scientific
       ( FPFormat(Fixed), Scientific, formatScientific, isInteger )
import           Data.Text as T
import           Data.Text.Encoding ( encodeUtf8 )

data Csv = Csv (HS.HashSet T.Text) [HM.HashMap T.Text T.Text]

instance Semigroup Csv where
    (Csv headers rows) <> (Csv headers' rows') =
        Csv (headers <> headers') (rows <> rows')

instance Monoid Csv where
    mempty = Csv mempty mempty

convert :: B.ByteString -> Either String B.ByteString
convert input = encode . toCsv <$> eitherDecode input

encode :: Csv -> B.ByteString
encode (Csv headers body) = encodeByName encodedHeaders encodedBody
  where
    encodedHeaders = header $ encodeUtf8 <$> HS.toList headers

    emptyRow = const mempty <$> toMap headers

    encodedBody = flip HM.union emptyRow <$> body

toCsv :: Value -> Csv
toCsv (Array array) = foldMap toCsv array
toCsv (Bool value) = Csv (HS.singleton $ boolToText value) []
toCsv Null = mempty
toCsv (Number value) = Csv (HS.singleton $ sciToText value) []
toCsv (Object object) = objectToCsv object
toCsv (String value) = Csv (HS.singleton value) []

objectToCsv :: Object -> Csv
objectToCsv = HM.foldlWithKey' merge (Csv mempty [ mempty ])
  where
    merge :: Csv -> T.Text -> Value -> Csv
    merge csv key (Bool value) = mergeField csv key (boolToText value)
    merge (Csv headers rows) key Null = Csv (HS.insert key headers) rows
    merge csv key (Number value) = mergeField csv key (sciToText value)
    merge csv key (String value) = mergeField csv key value
    merge csv key nestedValue = nest csv key (toCsv nestedValue)

    mergeField (Csv headers rows) key value =
        Csv (HS.insert key headers) (HM.insert key value <$> rows)

nest :: Csv -> Text -> Csv -> Csv
nest (Csv outerHeaders outerRows) key (Csv innerHeaders innerRows) =
    Csv
        (outerHeaders <> nestedInnerHeaders)
        [ HM.union outerRow innerRow
        | outerRow <- outerRows, innerRow <- nestedInnerRows
        ]
  where
    nestLabel label = key <> "->" <> label

    nestedInnerHeaders = HS.map nestLabel innerHeaders

    nestInnerRow =
        HM.foldlWithKey' (\innerRow key' value ->
                          HM.insert (nestLabel key') value innerRow) mempty

    nestedInnerRows = nestInnerRow <$> innerRows

boolToText :: Bool -> Text
boolToText False = "FALSE"
boolToText True = "TRUE"

sciToText :: Scientific -> Text
sciToText num = T.pack $ formatScientific Fixed decimalPlaces num
  where
    decimalPlaces =
        if isInteger num
            then Just 0
            else Nothing
