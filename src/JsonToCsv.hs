module JsonToCsv
  ( convert
  ) where

import           Data.Aeson ( Object, Value(..), eitherDecode )
import qualified Data.ByteString.Lazy as B
import           Data.Csv ( Header, NamedRecord, encodeByName, header )
import           Data.Csv.Incremental ( NamedBuilder, encodeNamedRecord )
import           Data.HashMap.Strict as HM
import           Data.HashSet as HS
import           Data.Text ( Text )
import           Data.Text.Encoding ( encodeUtf8 )

data Csv = Csv (HS.HashSet Text) [HM.HashMap Text Text]

instance Semigroup Csv where
    (Csv headers rows) <> (Csv headers' rows') =
        Csv (headers <> headers') (rows <> rows')

instance Monoid Csv where
    mempty = Csv mempty mempty

data CsvRow = CsvRow (HS.HashSet Text) (HM.HashMap Text Text)

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
encode (Csv headers body) = encodeByName encodedHeaders body
  where
    encodedHeaders = header $ encodeUtf8 <$> HS.toList headers

toCsv :: Value -> Csv
toCsv (Object object) = objectToCsv object
toCsv _ = Csv mempty mempty

objectToCsv :: Object -> Csv
objectToCsv object = csvRowToCsv $ HM.foldlWithKey' merge mempty object
  where
    merge :: CsvRow -> Text -> Value -> CsvRow
    merge (CsvRow headers row) key (String value) =
        CsvRow (HS.insert key headers) (HM.insert key value row)
