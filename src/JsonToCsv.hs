{-# LANGUAGE OverloadedStrings #-}

module JsonToCsv
  ( convert
  ) where

import           Data.Aeson ( Array, Object, Value(..), eitherDecode )
import qualified Data.ByteString.Lazy as B
import           Data.Csv ( encodeByName, header )
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import           Data.Scientific
       ( FPFormat(Fixed), Scientific, formatScientific, isInteger )
import qualified Data.Text as T
import           Data.Text.Encoding ( encodeUtf8 )
import qualified Data.Vector as V

data Csv = Csv (HS.HashSet T.Text) [HM.HashMap T.Text T.Text]
    deriving Show

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

    emptyRow = const mempty <$> HS.toMap headers

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
    merge csv key (Array array) = nestArray key csv array
    merge csv key (Bool value) = mergeField csv key (boolToText value)
    merge (Csv headers rows) key Null = Csv (HS.insert key headers) rows
    merge csv key (Number value) = mergeField csv key (sciToText value)
    merge csv key (Object object) = nestObject csv key (objectToCsv object)
    merge csv key (String value) = mergeField csv key value

    mergeField (Csv headers rows) key value =
        Csv (HS.insert key headers) (HM.insert key value <$> rows)

nestArray :: T.Text -> Csv -> Array -> Csv
nestArray key initialCsv initialArray =
    nestObject
        csvWithNonObjects
        key
        (toCsv . Array . fmap Object $ nestedObjects)
  where
    ( csvWithNonObjects, nestedObjects ) =
        V.ifoldl' merge ( initialCsv, mempty ) initialArray

    merge :: (Csv, V.Vector Object) -> Int -> Value -> (Csv, V.Vector Object)
    merge ( csv, objects ) index (Array array) =
        ( nestArray (fieldKey index) csv array, objects )
    merge ( csv, objects ) index (Bool value) =
        ( mergeField csv index (boolToText value), objects )
    merge ( csv, objects ) _ (Object object) = ( csv, V.snoc objects object )
    merge ( csv, objects ) index Null = ( mergeField csv index "", objects )
    merge ( csv, objects ) index (Number value) =
        ( mergeField csv index (sciToText value), objects )
    merge ( csv, objects ) index (String value) =
        ( mergeField csv index value, objects )

    fieldKey index = key <> "[" <> (T.pack . show $ index) <> "]"

    mergeField (Csv headers rows) index value =
        Csv
            (HS.insert (fieldKey index) headers)
            (HM.insert (fieldKey index) value <$> rows)

nestObject :: Csv -> T.Text -> Csv -> Csv
nestObject (Csv outerHeaders outerRows) _ (Csv _ []) =
    Csv outerHeaders outerRows
nestObject (Csv outerHeaders outerRows) key (Csv innerHeaders innerRows) =
    Csv
        (outerHeaders <> nestedInnerHeaders)
        [ HM.union outerRow innerRow
        | outerRow <- outerRows, innerRow <- nestedInnerRows
        ]
  where
    nestLabel label = key <> "." <> label

    nestedInnerHeaders = HS.map nestLabel innerHeaders

    nestInnerRow =
        HM.foldlWithKey' (\innerRow key' value ->
                          HM.insert (nestLabel key') value innerRow) mempty

    nestedInnerRows = nestInnerRow <$> innerRows

boolToText :: Bool -> T.Text
boolToText False = "FALSE"
boolToText True = "TRUE"

sciToText :: Scientific -> T.Text
sciToText num = T.pack $ formatScientific Fixed decimalPlaces num
  where
    decimalPlaces =
        if isInteger num
            then Just 0
            else Nothing
