{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Xlsx.Tabulate as XT
import qualified Data.HashMap.Strict as HM


main :: IO ()
main = do
  let path = "test/output.xlsx"
  XT.writeTable path table
  putStrLn ("Output is written to " <> path)


table :: XT.SimpleTable
table = XT.SimpleTable
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
