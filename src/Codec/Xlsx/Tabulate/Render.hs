{-# LANGUAGE RankNTypes #-}

module Codec.Xlsx.Tabulate.Render where

import qualified Codec.Xlsx as X
import Codec.Xlsx.Tabulate.Types (
  SimpleTable (SimpleTable),
  SimpleTableCellValue (..),
  SimpleTableColumns,
  SimpleTableRecord,
  stcvNone,
 )
import Control.Lens ((?~))
import qualified Control.Lens as L
import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Data.Scientific as S
import qualified Data.Text as T
import Data.Time (UTCTime (UTCTime))
import Data.Time.Clock.POSIX (getPOSIXTime)


render :: SimpleTable -> X.Worksheet -> (X.Worksheet, X.Range)
render = renderXY 1 1


renderXY :: Int -> Int -> SimpleTable -> X.Worksheet -> (X.Worksheet, X.Range)
renderXY r c (SimpleTable cols recs) w =
  let
    (nw, nr) = foldl (renderRecord cols c) (renderHeader w r c cols, r + 1) recs
   in
    (nw, X.mkRange (X.RowIndex r, X.ColumnIndex c) (X.RowIndex (nr - 1), X.ColumnIndex (length cols)))


renderHeader :: X.Worksheet -> Int -> Int -> SimpleTableColumns -> X.Worksheet
renderHeader w r c cols = fst $ foldl go (w, c) cols
  where
    go :: (X.Worksheet, Int) -> (T.Text, T.Text) -> (X.Worksheet, Int)
    go (wx, cx) (_, x) = (wx & X.cellValueAt (X.RowIndex r, X.ColumnIndex cx) ?~ X.CellText x, cx + 1)


renderRecord :: SimpleTableColumns -> Int -> (X.Worksheet, Int) -> SimpleTableRecord -> (X.Worksheet, Int)
renderRecord cols c (w, r) record =
  let
    cells = zip3 (repeat r) [c ..] $ fromMaybe stcvNone . flip HM.lookup record <$> fmap fst cols
    nw = foldl renderCell w cells
   in
    (nw, r + 1)


renderCell :: X.Worksheet -> (Int, Int, SimpleTableCellValue) -> X.Worksheet
renderCell w (r, c, cell) =
  case cell of
    STCVNone -> w
    (STCVBool x) -> w & X.cellValueAt (X.RowIndex r, X.ColumnIndex c) ?~ X.CellBool x
    (STCVDate x) -> w & X.cellValueAt (X.RowIndex r, X.ColumnIndex c) ?~ X.CellDouble (X.dateToNumber X.DateBase1900 (UTCTime x 0))
    (STCVText x) -> w & X.cellValueAt (X.RowIndex r, X.ColumnIndex c) ?~ X.CellText x
    (STCVNumb x) -> w & X.cellValueAt (X.RowIndex r, X.ColumnIndex c) ?~ X.CellDouble (S.toRealFloat x)
    (STCVForm x) -> w & cellFormulaAt (r, c) ?~ X.CellFormula (X.NormalFormula (X.Formula x)) False False


cellFormulaAt :: (Int, Int) -> L.Lens' X.Worksheet (Maybe X.CellFormula)
cellFormulaAt (r, c) = X.atCell (X.RowIndex r, X.ColumnIndex c) . L.non X.def . X.cellFormula


writeTable :: FilePath -> SimpleTable -> IO ()
writeTable filepath st = do
  (content, _) <- writeBL st
  BL.writeFile filepath content


writeBL :: SimpleTable -> IO (BL.ByteString, X.Range)
writeBL st = do
  ct <- getPOSIXTime
  let (ws, range) = render st X.def
  pure (X.fromXlsx ct (X.def & X.atSheet (T.pack "Sheet 1") ?~ ws), range)
