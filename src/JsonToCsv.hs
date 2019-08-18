module JsonToCsv
  ( convert
  ) where

import           Data.Aeson ( Object, Value(..), eitherDecode )
import qualified Data.ByteString.Lazy as B
import           Data.Csv ( Header, NamedRecord, encodeByName, header )
import           Data.Csv.Incremental ( NamedBuilder, encodeNamedRecord )
import           Data.HashMap.Strict as HM
import           Data.HashSet as HS
import           Data.Text as T
import           Data.Text.Encoding ( encodeUtf8 )

data Csv = Csv (HS.HashSet T.Text) [HM.HashMap T.Text T.Text]

instance Semigroup Csv where
    (Csv headers rows) <> (Csv headers' rows') =
        Csv (headers <> headers') (rows <> rows')

instance Monoid Csv where
    mempty = Csv mempty mempty

data CsvRow = CsvRow (HS.HashSet T.Text) (HM.HashMap T.Text T.Text)

instance Semigroup CsvRow where
    (CsvRow headers row) <> (CsvRow headers' row') =
        CsvRow (headers <> headers') (row <> row')

instance Monoid CsvRow where
    mempty = CsvRow mempty mempty

csvRowToCsv :: CsvRow -> Csv
csvRowToCsv (CsvRow headers row) = Csv headers [ row ]

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
toCsv (Object object) = objectToCsv object

objectToCsv :: Object -> Csv
objectToCsv object = csvRowToCsv $ HM.foldlWithKey' merge mempty object
  where
    merge :: CsvRow -> T.Text -> Value -> CsvRow
    merge (CsvRow headers row) key (Bool value) =
        CsvRow
            (HS.insert key headers)
            (HM.insert key (T.pack . show $ value) row)
    merge (CsvRow headers row) key Null =
        CsvRow (HS.insert key headers) (HM.insert key mempty row)
    merge (CsvRow headers row) key (Number value) =
        CsvRow
            (HS.insert key headers)
            (HM.insert key (T.pack . show $ value) row)
    merge (CsvRow headers row) key (String value) =
        CsvRow (HS.insert key headers) (HM.insert key value row)
