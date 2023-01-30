{-# LANGUAGE RankNTypes #-}

module Codec.Xlsx.Tabulate.Types where

import qualified Codec.Xlsx.Formatted as XF
import qualified Data.HashMap.Strict as HM
import qualified Data.Scientific as S
import qualified Data.Text as T
import Data.Time (Day)


data SimpleTable = SimpleTable
  { simpleTableColumns :: !SimpleTableColumns
  , simpleTableRecords :: ![SimpleTableRecord]
  }
  deriving (Show)


data SimpleFormatTable = SimpleFormatTable
  { simpleFormatTableColumns :: !SimpleTableColumns
  , simpleFormatTableFormats :: !SimpleTableFormats
  , simpleFormatTableRecords :: ![SimpleTableRecord]
  }


type SimpleTableColumnKey = T.Text


type SimpleTableColumns = [(SimpleTableColumnKey, T.Text)]


type SimpleTableFormats = HM.HashMap SimpleTableColumnKey (XF.FormattedCell -> XF.FormattedCell)


type SimpleTableRecord = HM.HashMap SimpleTableColumnKey SimpleTableCellValue


data SimpleTableCellValue
  = STCVNone
  | STCVBool !Bool
  | STCVDate !Day
  | STCVText !T.Text
  | STCVNumb !S.Scientific
  | STCVForm !T.Text
  deriving (Show)


stcvNone :: SimpleTableCellValue
stcvNone = STCVNone


stcvBool :: Bool -> SimpleTableCellValue
stcvBool = STCVBool


stcvDate :: Day -> SimpleTableCellValue
stcvDate = STCVDate


stcvText :: T.Text -> SimpleTableCellValue
stcvText = STCVText


stcvNumb :: S.Scientific -> SimpleTableCellValue
stcvNumb = STCVNumb


stcvForm :: T.Text -> SimpleTableCellValue
stcvForm = STCVForm
