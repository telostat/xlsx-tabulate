{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Xlsx           as X
import qualified Codec.Xlsx.Formatted as XF
import           Codec.Xlsx.Tabulate  (preformat)
import qualified Codec.Xlsx.Tabulate  as XT
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as HM


main :: IO ()
main = do
  let path_simple = "test/output_simple.xlsx"
  XT.writeTable path_simple tableSimple
  putStrLn ("Output is written to " <> path_simple)

  let path_formatted = "test/output_formatted.xlsx"
  XT.writeFormatTable path_formatted tableFormatted
  putStrLn ("Output is written to " <> path_formatted)


tableSimple :: XT.SimpleTable
tableSimple = XT.SimpleTable
  { XT.simpleTableColumns = columns
  , XT.simpleTableRecords = fmap HM.fromList records
  }
  where
    columns =
      [ ("name", "Name")
      , ("number", "Number")
      ]
    records =
      [ [("name", XT.stcvText "Hebele"), ("number", XT.stcvNumb 1234567.1234)]
      , [("name", XT.stcvText "Hubele"), ("number", XT.stcvNumb 2345678.5678)]
      ]


tableFormatted :: XT.SimpleFormatTable
tableFormatted = XT.SimpleFormatTable
  { XT.simpleFormatTableColumns = columns
  , XT.simpleFormatTableFormats = HM.fromList formats
  , XT.simpleFormatTableRecords = fmap HM.fromList records
  }
  where
    columns =
      [ ("name", "Name")
      , ("number", "Number")
      ]
    formats =
      [ ("name", XT.withAlignC)
      , ("number", XT.withFmtNumb "#,###.##")
      ]
    records =
      [ [("name", XT.stcvText "Hebele"), ("number", XT.stcvNumb 1234567.1234)]
      , [("name", XT.stcvText "Hubele"), ("number", XT.stcvNumb 2345678.5678)]
      ]
