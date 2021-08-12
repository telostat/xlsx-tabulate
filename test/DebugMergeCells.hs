{-# LANGUAGE OverloadedStrings #-}

module DebugMergeCells where

import qualified Codec.Xlsx            as X
import qualified Codec.Xlsx.Formatted  as XF
import qualified Codec.Xlsx.Tabulate   as XT
import           Control.Lens          ((?~))
import qualified Control.Lens          as L
import qualified Data.ByteString.Lazy  as BL
import           Data.Function         ((&))
import qualified Data.Map.Strict       as HM
import qualified Data.Text             as T
import           Data.Time.Clock.POSIX (getPOSIXTime)


debugMergeCells :: XT.SimpleFormatTable -> XT.SimpleFormatTable -> IO BL.ByteString
debugMergeCells sft1 sft2 = do
  ct <- getPOSIXTime
  let (fcm1, range) = XT.preformatWithTitleXY 1 1 id "Deneme1" sft1
  let (fcm2, range) = XT.preformatWithTitleXY 20 1 id "Deneme2" sft2
  let xlsx = XF.formatWorkbook [("Sheet 1", fcm1), ("Sheet 2", fcm2)] X.minimalStyleSheet
  print xlsx
  pure (X.fromXlsx ct xlsx)


-- addHeader
--   :: Int -- ^ The row to put the header at.
--   -> Int -- ^ The column to put the header at.
--   -> Int -- ^ Number of
--   -> (XF.FormattedCell -> XF.FormattedCell)
--   -> T.Text
--   -> HM.Map (Int, Int) XF.FormattedCell
--   -> HM.Map (Int, Int) XF.FormattedCell
-- addHeader row col colspan formatter text cellmap = withColspan n . fmtHead
--   where
--     fillp = X.def & X.fillPatternBgColor ?~ (X.def & X.colorARGB ?~ "FF000080")
--     fmtBold = XF.formattedFormat . XF.formatFont . L.non X.def . X.fontBold ?~ True
--     fmtItal = XF.formattedFormat . XF.formatFont . L.non X.def . X.fontItalic ?~ True
--     fmtBcol = XF.formattedFormat . XF.formatFill . L.non X.def . X.fillPattern ?~ fillp
--     fmtHead = fmtBold . fmtItal . fmtBcol
