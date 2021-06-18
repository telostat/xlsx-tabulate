{-# LANGUAGE RankNTypes #-}

module Codec.Xlsx.Tabulate.Internal where

import qualified Codec.Xlsx            as X
import           Control.Lens          ((?~))
import qualified Control.Lens          as L
import qualified Data.ByteString.Lazy  as BL
import           Data.Function         ((&))
import qualified Data.HashMap.Strict   as HM
import           Data.Maybe            (fromMaybe)
import qualified Data.Scientific       as S
import qualified Data.Text             as T
import           Data.Time.Clock.POSIX (getPOSIXTime)


data SimpleTable = SimpleTable
  { simpleTableColumns :: !SimpleTableColumns
  , simpleTableRecords :: ![SimpleTableRecord]
  } deriving (Show)


type SimpleTableColumnKey = T.Text


type SimpleTableColumns = [(SimpleTableColumnKey, T.Text)]


type SimpleTableRecord = HM.HashMap SimpleTableColumnKey SimpleTableCellValue


data SimpleTableCellValue =
    STCVNone
  | STCVBool !Bool
  | STCVDate !T.Text
  | STCVText !T.Text
  | STCVNumb !S.Scientific
  | STCVForm !T.Text
  deriving (Show)


stcvNone :: SimpleTableCellValue
stcvNone = STCVNone


stcvBool :: Bool -> SimpleTableCellValue
stcvBool = STCVBool


stcvDate :: T.Text -> SimpleTableCellValue
stcvDate = STCVDate


stcvText :: T.Text -> SimpleTableCellValue
stcvText = STCVText


stcvNumb :: S.Scientific -> SimpleTableCellValue
stcvNumb = STCVNumb


stcvForm :: T.Text -> SimpleTableCellValue
stcvForm = STCVForm


render :: SimpleTable -> X.Worksheet -> (X.Worksheet, X.Range)
render = renderXY 1 1


renderXY :: Int -> Int -> SimpleTable -> X.Worksheet -> (X.Worksheet, X.Range)
renderXY r c (SimpleTable cols recs) w =
  let
    (nw, nr) = foldl (renderRecord cols c) (renderHeader w r c cols, r + 1) recs
  in
    (nw, X.mkRange (r, c) (nr - 1, length cols))


renderHeader :: X.Worksheet -> Int -> Int -> SimpleTableColumns -> X.Worksheet
renderHeader w r c cols = fst $ foldl go (w, c) cols
  where
    go :: (X.Worksheet, Int) -> (T.Text, T.Text) -> (X.Worksheet, Int)
    go (wx, cx) (_, x) = (wx & X.cellValueAt (r, cx) ?~ X.CellText x, cx + 1)


renderRecord :: SimpleTableColumns -> Int -> (X.Worksheet, Int) -> SimpleTableRecord -> (X.Worksheet, Int)
renderRecord cols c (w, r) record =
  let
    cells = zip3 (repeat r) [c..] $ fromMaybe stcvNone . flip HM.lookup record <$> fmap fst cols
    nw = foldl renderCell w cells
  in
    (nw, r + 1)


renderCell :: X.Worksheet -> (Int, Int, SimpleTableCellValue) -> X.Worksheet
renderCell w (r, c, cell) =
  case cell of
    STCVNone     -> w
    (STCVBool x) -> w & X.cellValueAt (r, c) ?~ X.CellBool x
    (STCVDate x) -> w & X.cellValueAt (r, c) ?~ X.CellText x
    (STCVText x) -> w & X.cellValueAt (r, c) ?~ X.CellText x
    (STCVNumb x) -> w & X.cellValueAt (r, c) ?~ X.CellDouble (S.toRealFloat x)
    (STCVForm x) -> w & cellFormulaAt (r, c) ?~ X.CellFormula (X.NormalFormula (X.Formula x)) False False


cellFormulaAt :: (Int, Int) -> L.Lens' X.Worksheet (Maybe X.CellFormula)
cellFormulaAt i = X.atCell i . L.non X.def . X.cellFormula


writeTable :: FilePath -> SimpleTable -> IO ()
writeTable filepath st = do
  (content, range) <- writeBL st
  BL.writeFile filepath content
  putStrLn . T.unpack $ "Table range is " <> X.unCellRef range <> "."


writeBL :: SimpleTable -> IO (BL.ByteString, X.Range)
writeBL st = do
  ct <- getPOSIXTime
  let (ws, range) = render st X.def
  pure (X.fromXlsx ct (X.def & X.atSheet "Sheet 1" ?~ ws), range)


example :: SimpleTable
example = SimpleTable
  [("a", "A"), ("b", "B"), ("e", "E"), ("c", "C")]
  [ HM.fromList [("a", stcvText "A1"), ("b", stcvText "B1"), ("c", stcvText "C1")]
  , HM.fromList [("a", stcvText "A2"), ("b", stcvText "B2"), ("c", stcvText "C2")]
  ]
