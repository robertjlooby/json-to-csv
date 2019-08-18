module Main where

import qualified Data.ByteString.Lazy as B
import           System.Exit ( exitFailure )
import           System.IO ( hPutStr, stderr )

import           JsonToCsv

main :: IO ()
main = do
    input <- B.getContents
    either errorAndExit B.putStr (convert input)

errorAndExit :: String -> IO ()
errorAndExit msg = hPutStr stderr msg >> exitFailure
