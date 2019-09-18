{-# LANGUAGE OverloadedStrings #-}

module JsonToCsv
  ( convert
  ) where

import           Data.Aeson ( Array, Object, Value(..), eitherDecode )
import qualified Data.ByteString.Lazy as B
import           Data.Csv ( encodeByName, header )
import           Data.Foldable ( foldl', toList )
import qualified Data.HashMap.Strict as HM
import           Data.Scientific
       ( FPFormat(Fixed), Scientific, formatScientific, isInteger )
import qualified Data.Set.Ordered as HS
import qualified Data.Text as T
import           Data.Text.Encoding ( encodeUtf8 )
import qualified Data.Vector as V
import           Debug.Trace

data Csv = Csv (HS.OSet T.Text) [HM.HashMap T.Text T.Text]
    deriving Show

instance Semigroup Csv where
    (Csv headers rows) <> (Csv headers' rows') =
        Csv (headers HS.|<> headers') (rows <> rows')

instance Monoid Csv where
    mempty = Csv HS.empty mempty

convert :: B.ByteString -> Either String B.ByteString
convert input = encode . toCsv <$> eitherDecode input

encode :: Csv -> B.ByteString
encode (Csv headers rows) =
    encodeByName
        (trace ("encoded headers: " <> show encodedHeaders) encodedHeaders)
        encodedRows
  where
    encodedHeaders = header $ encodeUtf8 <$> toList headers

    emptyRow = foldl' (\hm k -> HM.insert k mempty hm) mempty headers

    encodedRows = flip HM.union emptyRow <$> rows

toCsv :: Value -> Csv
toCsv (Array array) = nestArray mempty (Csv HS.empty [ mempty ]) array
toCsv (Bool value) = Csv (HS.singleton $ boolToText value) []
toCsv Null = mempty
toCsv (Number value) = Csv (HS.singleton $ sciToText value) []
toCsv (Object object) = objectToCsv object
toCsv (String value) = Csv (HS.singleton value) []

objectToCsv :: Object -> Csv
objectToCsv = HM.foldlWithKey' merge (Csv HS.empty [ mempty ])
  where
    merge :: Csv -> T.Text -> Value -> Csv
    merge csv key (Array array) = nestArray key csv array
    merge csv key (Bool value) = mergeField csv key (boolToText value)
    merge (Csv headers rows) key Null = Csv (key HS.<| headers) rows
    merge csv key (Number value) = mergeField csv key (sciToText value)
    merge csv key (Object object) = nestObject csv key (objectToCsv object)
    merge csv key (String value) = mergeField csv key value

    mergeField (Csv headers rows) key value =
        Csv (key HS.<| headers) (HM.insert key value <$> rows)

nestArray :: T.Text -> Csv -> Array -> Csv
nestArray key initialCsv initialArray =
    nestObject csvWithNonObjects key (foldMap objectToCsv nestedObjects)
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
            ((fieldKey index) HS.<| headers)
            (HM.insert (fieldKey index) value <$> rows)

osetMap :: Ord b => (a -> b) -> HS.OSet a -> HS.OSet b
osetMap f = foldl' (\hs a -> hs HS.|> (f a)) HS.empty

nestObject :: Csv -> T.Text -> Csv -> Csv
nestObject (Csv outerHeaders outerRows) key (Csv innerHeaders innerRows)
  | null innerRows = Csv nestedHeaders outerRows
  | null outerRows = Csv nestedHeaders nestedInnerRows
  | otherwise =
      Csv
          nestedHeaders
          [ HM.union outerRow innerRow
          | outerRow <- outerRows, innerRow <- nestedInnerRows
          ]
  where
    nestLabel label = case key of
        "" -> label -- Don't prefix headers with `.`s for a top level array
        _ -> key <> "." <> label

    nestedHeaders = outerHeaders HS.|<> osetMap nestLabel innerHeaders

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
