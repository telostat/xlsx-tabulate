{-# LANGUAGE TupleSections #-}

module Codec.Xlsx.Tabulate.RenderFormat where

import qualified Codec.Xlsx as X
import qualified Codec.Xlsx.Formatted as XF
import Codec.Xlsx.Tabulate.Types (
  SimpleFormatTable (SimpleFormatTable),
  SimpleTableCellValue (..),
  SimpleTableColumns,
  SimpleTableFormats,
  SimpleTableRecord,
  stcvNone,
 )
import Control.Lens ((.~), (?~))
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map, fromList, insert, toList, union)
import Data.Maybe (fromMaybe)
import Data.Scientific (toRealFloat)
import qualified Data.Text as T
import Data.Time (UTCTime (UTCTime))
import Data.Time.Clock.POSIX (getPOSIXTime)


preformat :: SimpleFormatTable -> (Map (Int, Int) XF.FormattedCell, X.Range)
preformat = preformatXY 1 1


preformatXY :: Int -> Int -> SimpleFormatTable -> (Map (Int, Int) XF.FormattedCell, X.Range)
preformatXY r c (SimpleFormatTable cols fmts recs) =
  let
    (res, nr) = foldl (preformatRecord cols fmts c) (preformatHeader r c cols, r + 1) recs
   in
    (res, X.mkRange (X.RowIndex r, X.ColumnIndex c) (X.RowIndex (nr - 1), X.ColumnIndex (length cols)))


preformatHeader :: Int -> Int -> SimpleTableColumns -> Map (Int, Int) XF.FormattedCell
preformatHeader r c cols = fromList (fmap (\(idx, (_, n)) -> ((r, idx), X.def & withValue (X.CellText n))) (zip [c ..] cols))


preformatWithTitleXY
  :: Int
  -- ^ Row number to put the table at.
  -> Int
  -- ^ Column number to put the table at.
  -> (XF.FormattedCell -> XF.FormattedCell)
  -- ^ Table title formatter.
  -> T.Text
  -- ^ Title of the table.
  -> SimpleFormatTable
  -> (Map (Int, Int) XF.FormattedCell, X.Range)
preformatWithTitleXY r c formatter title (SimpleFormatTable cols fmts recs) =
  let
    -- Get the table width:
    width = length cols

    -- Create the title of the table:
    titleCell = formatter (X.def & withValue (X.CellText title) & XF.formattedColSpan .~ width)

    -- Create the header and contents of the table:
    (contentCells, nr) = foldl (preformatRecord cols fmts c) (preformatHeader (r + 1) c cols, r + 2) recs
   in
    (insert (r, c) titleCell contentCells, X.mkRange (X.RowIndex r, X.ColumnIndex c) (X.RowIndex (nr - 1), X.ColumnIndex (length cols)))


preformatRecord
  :: SimpleTableColumns
  -> SimpleTableFormats
  -> Int
  -> (Map (Int, Int) XF.FormattedCell, Int)
  -> SimpleTableRecord
  -> (Map (Int, Int) XF.FormattedCell, Int)
preformatRecord cols fmts c (sofar, r) record =
  let
    cvals = fmap (fromMaybe stcvNone . flip HM.lookup record . fst) cols
    cfmts = fmap (fromMaybe id . flip HM.lookup fmts . fst) cols
    cindx = fmap (r,) [c ..]
    ccont = fmap (\(idx, fmt, val) -> (idx, X.def & withCellValue val & fmt)) (zip3 cindx cfmts cvals)
   in
    (sofar `union` fromList ccont, r + 1)


-- * Helpers


-- &helpers

withValue :: X.CellValue -> XF.FormattedCell -> XF.FormattedCell
withValue x = XF.formattedCell . X.cellValue ?~ x


withFormula :: X.CellFormula -> XF.FormattedCell -> XF.FormattedCell
withFormula x = XF.formattedCell . X.cellFormula ?~ x


withCellValue :: SimpleTableCellValue -> XF.FormattedCell -> XF.FormattedCell
withCellValue STCVNone = id
withCellValue (STCVBool x) = withValue (X.CellBool x)
withCellValue (STCVDate x) = withValue (X.CellDouble (X.dateToNumber X.DateBase1900 (UTCTime x 0)))
withCellValue (STCVText x) = withValue (X.CellText x)
withCellValue (STCVNumb x) = withValue (X.CellDouble (toRealFloat x))
withCellValue (STCVForm x) = withFormula (X.CellFormula (X.NormalFormula (X.Formula x)) False False)


withFmtNumb :: T.Text -> XF.FormattedCell -> XF.FormattedCell
withFmtNumb fmt = XF.formattedFormat . XF.formatNumberFormat ?~ X.UserNumberFormat fmt


withFmtText :: XF.FormattedCell -> XF.FormattedCell
withFmtText = XF.formattedFormat . XF.formatNumberFormat ?~ X.StdNumberFormat X.NfTextPlaceHolder


withFmtDate :: T.Text -> XF.FormattedCell -> XF.FormattedCell
withFmtDate fmt = XF.formattedFormat . XF.formatNumberFormat ?~ X.UserNumberFormat fmt


withFmtTime :: T.Text -> XF.FormattedCell -> XF.FormattedCell
withFmtTime fmt = XF.formattedFormat . XF.formatNumberFormat ?~ X.UserNumberFormat fmt


withAlignL :: XF.FormattedCell -> XF.FormattedCell
withAlignL = XF.formattedFormat . XF.formatAlignment . L.non X.def . X.alignmentHorizontal ?~ X.CellHorizontalAlignmentLeft


withAlignC :: XF.FormattedCell -> XF.FormattedCell
withAlignC = XF.formattedFormat . XF.formatAlignment . L.non X.def . X.alignmentHorizontal ?~ X.CellHorizontalAlignmentCenter


withAlignR :: XF.FormattedCell -> XF.FormattedCell
withAlignR = XF.formattedFormat . XF.formatAlignment . L.non X.def . X.alignmentHorizontal ?~ X.CellHorizontalAlignmentRight


writeFormatTable :: FilePath -> SimpleFormatTable -> IO ()
writeFormatTable filepath st = writeFormatBL st >>= BL.writeFile filepath


writeFormatBL :: SimpleFormatTable -> IO BL.ByteString
writeFormatBL st = do
  ct <- getPOSIXTime
  let (cellmap, _) = preformat st
  let xlsx = XF.formatWorkbook [(T.pack "Sheet 1", rcToRC cellmap)] X.minimalStyleSheet
  pure (X.fromXlsx ct xlsx)


rcToRC :: Map (Int, Int) XF.FormattedCell -> Map (X.RowIndex, X.ColumnIndex) XF.FormattedCell
rcToRC x = fromList $ fmap (\((r, c), v) -> ((X.RowIndex r, X.ColumnIndex c), v)) (toList x)
