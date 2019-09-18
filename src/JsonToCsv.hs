{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonToCsv
  ( convert
  ) where

import           Data.Aeson ( Array, Object, Value(..), eitherDecode )
import qualified Data.ByteString.Lazy as B
import qualified Data.Csv as Cassava
import qualified Data.HashMap.Strict as HM
import           Data.Hashable ( Hashable )
import           Data.Scientific
       ( FPFormat(Fixed), Scientific, formatScientific, isInteger )
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text.Encoding ( encodeUtf8 )
import qualified Data.Vector as V
import           GHC.Generics ( Generic )

data Header
    = Key T.Text Header
    | Index Int Header
    | NilHeader
    deriving ( Generic, Eq, Ord, Show )

instance Semigroup Header where
    NilHeader <> header = header
    header <> NilHeader = header
    Key key header <> header' = Key key (header <> header')
    Index i header <> header' = Index i (header <> header')

instance Monoid Header where
    mempty = NilHeader

instance Hashable Header

instance Cassava.ToField Header where
    toField = encodeUtf8 . headerToText

class AsHeader a where
    asHeader :: a -> Header

instance AsHeader Int where
    asHeader = flip Index NilHeader

instance AsHeader T.Text where
    asHeader = flip Key NilHeader

headerToText :: Header -> T.Text
headerToText = T.dropWhile (== '.') . headerToText'
  where
    headerToText' :: Header -> T.Text
    headerToText' (Key key rest) = "." <> key <> headerToText' rest
    headerToText' (Index i rest) =
        "[" <> (T.pack . show $ i) <> "]" <> headerToText' rest
    headerToText' NilHeader = ""

data Csv = Csv (S.Set Header) [HM.HashMap Header T.Text]
    deriving Show

instance Semigroup Csv where
    (Csv headers rows) <> (Csv headers' rows') =
        Csv (headers <> headers') (rows <> rows')

instance Monoid Csv where
    mempty = Csv mempty mempty

convert :: B.ByteString -> Either String B.ByteString
convert input = encode . toCsv <$> eitherDecode input

encode :: Csv -> B.ByteString
encode (Csv headers rows) = Cassava.encodeByName encodedHeaders encodedRows
  where
    encodedHeaders =
        Cassava.header . fmap Cassava.toField . S.toAscList $ headers

    emptyRow = S.foldl' (\hm k -> HM.insert k mempty hm) mempty headers

    encodedRows = flip HM.union emptyRow <$> rows

toCsv :: Value -> Csv
toCsv (Array array) = nestArray mempty (Csv mempty [ mempty ]) array
toCsv (Bool value) = Csv (S.singleton . asHeader . boolToText $ value) []
toCsv Null = mempty
toCsv (Number value) = Csv (S.singleton . asHeader . sciToText $ value) []
toCsv (Object object) = objectToCsv object
toCsv (String value) = Csv (S.singleton . asHeader $ value) []

objectToCsv :: Object -> Csv
objectToCsv = HM.foldlWithKey' merge (Csv mempty [ mempty ])
  where
    merge :: Csv -> T.Text -> Value -> Csv
    merge csv key (Array array) = nestArray (asHeader key) csv array
    merge csv key (Bool value) = mergeField csv key (boolToText value)
    merge (Csv headers rows) key Null =
        Csv (S.insert (asHeader key) headers) rows
    merge csv key (Number value) = mergeField csv key (sciToText value)
    merge csv key (Object object) =
        nestObject csv (asHeader key) (objectToCsv object)
    merge csv key (String value) = mergeField csv key value

    mergeField (Csv headers rows) key value =
        Csv
            (S.insert (asHeader key) headers)
            (HM.insert (asHeader key) value <$> rows)

nestArray :: Header -> Csv -> Array -> Csv
nestArray header initialCsv initialArray =
    nestObject csvWithNonObjects header (foldMap objectToCsv nestedObjects)
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

    fieldKey index = header <> (asHeader index)

    mergeField (Csv headers rows) index value =
        Csv
            (S.insert (fieldKey index) headers)
            (HM.insert (fieldKey index) value <$> rows)

nestObject :: Csv -> Header -> Csv -> Csv
nestObject (Csv outerHeaders outerRows) header (Csv innerHeaders innerRows)
  | null innerRows = Csv nestedHeaders outerRows
  | null outerRows = Csv nestedHeaders nestedInnerRows
  | otherwise =
      Csv
          nestedHeaders
          [ HM.union outerRow innerRow
          | outerRow <- outerRows, innerRow <- nestedInnerRows
          ]
  where
    nestLabel label = header <> label

    nestedHeaders = outerHeaders <> S.map nestLabel innerHeaders

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
