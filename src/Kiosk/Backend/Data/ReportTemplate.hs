{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


{- |
Module      :  Kiosk.Backend.Data.ReportTemplate
Description :  Render a Report Template from a Form and a list of DataTemplates
Copyright   :  Plow Technologies LLC
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

Data Templates and Form Helpers for making ReportTemplates

-}

module Kiosk.Backend.Data.ReportTemplate where

import           Codec.Xlsx
import           Control.Applicative             ((<$>), (<*>))
import           Control.Lens
import           Data.Map                        (Map)
import qualified Data.Map.Lazy                   as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Time
import           Kiosk.Backend.Data.DataTemplate
-- import           Kiosk.Backend.Data.DataTemplateEntry
import           Kiosk.Backend.Form
import           ReportTemplate.Internal
import           System.Locale
type KioskReportTemplate context preOut rowOut= ReportTemplate context Form preOut DataTemplate rowOut


makeLenses ''Company
makeLenses ''Report
makePrisms ''ReportTable
makeLenses ''ReportTableRowStyle
makeLenses ''ReportPreamble
makePrisms ''InputType
makeLenses ''InputText
makeLenses ''InputDouble
makeLenses ''InputDate

-- | Kiosk Specific
type KioskPreambleTemplateList context preOut= [(ReportPreambleLabel, context -> Form -> preOut)]
type KioskRowTemplateList context rowOut = [(ReportRowLabel, context -> DataTemplate -> rowOut)]
type KioskPreambleTemplate context preOut= ReportPreambleTemplate context Form preOut
type KioskRowTemplate context rowOut = ReportRowTemplate context DataTemplate rowOut


-- | Spreadsheet specific

data XlsxContext = XlsxContext {
                 _xlsxCurrentTime :: String}

type XlsxReportTemplate = KioskReportTemplate XlsxContext CellMap Cell
type XlsxPreambleTemplateList = KioskPreambleTemplateList XlsxContext CellMap
type XlsxRowTemplateList = KioskRowTemplateList XlsxContext Cell
type XlsxReport = Report CellMap Cell
type XlsxPreamble = ReportPreamble CellMap
type XlsxTable = ReportTable Cell




-- | Excel Form Rendering Helper Functions
-- Because the excel preamble is a full cell map
getCompanyName :: (Int,Int) -> Form -> CellMap
getCompanyName key form = makeCellMapFromText key companyName
  where
    companyName = form ^. getCompany.getCompanyText

makeCellMapFromText :: (Int,Int) -> Text -> CellMap
makeCellMapFromText key t = M.insert key cellText M.empty
  where
    cellText = def & cellValue .~ (Just . CellText $ t)

makeCellMapFromUTCTime ::  String -> (Int, Int) -> UTCTime -> CellMap
makeCellMapFromUTCTime timeFormatString key  = makeCellMapFromText key .
                                               T.pack .
                                               formatTime defaultTimeLocale
                                                          timeFormatString




-- | Row Rendering Helper Functions

-- | Retrieve Cell Data
makeCellDoubleFromInputDouble :: Text -> DataTemplate -> Cell
makeCellDoubleFromInputDouble = makeCellValueFromDataTemplate CellDouble inputDoubleLens
                               where
                                  inputDoubleLens = _InputTypeDouble.getInputDouble


makeCellTextWithCellTemplate :: ([Text] -> Text )
                                -> [Text] -> DataTemplate -> Cell
makeCellTextWithCellTemplate templateFcn txts dte = def & cellValue ?~ cellVal
 where
    cellVal = CellText . templateFcn $ targetTextList
    inputTextLens = _InputTypeText.getInputText
    targetTextList :: [Text]
    targetTextList = fromMaybe "" <$> (getInputTypeByLabel inputTextLens
                                       <$> txts
                                       <*> [dte])


makeCellDoubleWithCellTemplate :: ([Text] -> Either Text Double )
                                  -> [Text] -> DataTemplate -> Cell
makeCellDoubleWithCellTemplate templateFcn txts dte = def & cellValue ?~ cellVal
 where
    cellVal = either CellText CellDouble  $ templateFcn $ targetTextList
    inputTextLens = _InputTypeText.getInputText
    targetTextList :: [Text]
    targetTextList = fromMaybe "" <$> (getInputTypeByLabel inputTextLens
                                       <$> txts
                                       <*> [dte])

makeCellTextFromInputText :: Text -> DataTemplate -> Cell
makeCellTextFromInputText = makeCellValueFromDataTemplate CellText inputTextLens
                                       where
                                          inputTextLens = _InputTypeText.getInputText

makeCellTextFromInputDate :: Text -> DataTemplate -> Cell
makeCellTextFromInputDate l dte = def & cellValue .~ maybeCellValue
                           where
                              maybeInputDate = getInputTypeByLabel inputLens l$ dte
                              maybeCellValue = CellText <$> maybeInputDate
                              inputLens = _InputTypeDate . getInputDate



makeCellValueFromDataTemplate ::
  (s -> CellValue)
  -> Getting (First s) InputType s -> Text -> DataTemplate -> Cell
makeCellValueFromDataTemplate cellConstructor  lensDt l dt  = outputCell
  where
    maybeCellValue :: Maybe CellValue
    maybeCellValue = cellConstructor <$> (getInputTypeByLabel lensDt l $  dt)
    outputCell :: Cell
    outputCell = def  & cellValue .~ maybeCellValue


getInputTypeByLabel ::
  Getting (First a) InputType a -> Text -> DataTemplate -> Maybe a
getInputTypeByLabel lensDt l dt  = outputCell
  where
    singletonInput = catMaybes.
                    fmap (getItemMatchingLabel l lensDt) .
                    templateItems $ dt
    outputCell = case singletonInput of
                   [] -> Nothing
                   (x:_) -> Just  x

getItemMatchingLabel
  :: Text
     -> Getting (First a) InputType a
     -> TemplateItem
     -> Maybe a
getItemMatchingLabel l dtLens (TemplateItem lbl inVal)
 |l == lbl = inVal ^? dtLens
 |otherwise = Nothing


-- | Build 'Report' from 'ReportTemplate'

buildXlsxReport :: XlsxReportTemplate -> XlsxContext ->
                   Form ->
                   [DataTemplate] -> XlsxReport
buildXlsxReport xlsxReportTemplate xlsxContext form dataTemplates = renderedReport
  where
     renderedReport = renderReport xlsxReportTemplate xlsxContext form dataTemplates

-- | Create Excel Spreadsheet

-- | Render Spreadsheet from report
renderSpreadsheet :: XlsxReport -> Worksheet
renderSpreadsheet report = def & wsCells .~ combinedMap
  where
   combinedMap :: CellMap
   combinedMap = M.unions (preambleMapList ++ [labelCellMap] ++ rowMapList)
   preambleOffset = 10

   preambleMapList :: [CellMap]
   preambleMapList =  toListOf (reportPreamble.preambleValue.folded._2) report
   labelToIntMap :: Map ReportRowLabel Int
   labelToIntMap = M.fromList . zip (report ^. (reportRows . _ReportTableRowIndex . _1 )  ) $ [1..]
   rowMapList  :: [CellMap]
   rowMapList  = (foldrTableByRowWithIndex transformPositionAndMap M.empty) <$>
                  (toListOf  (reportRows._ReportTableRowIndex._2) report)
   transformPositionAndMap :: (Int,String) -> Cell -> CellMap -> CellMap
   transformPositionAndMap (rowInt,label') rowVal rowMap' =  case M.lookup label' labelToIntMap of
          Nothing -> rowMap'
          (Just i) -> M.insert (rowInt + preambleOffset , i)  rowVal rowMap'

   labelCellMap = M.foldrWithKey (\label' idx m -> M.insert (preambleOffset,idx) (convertText label') m )
                                 M.empty
                                 labelToIntMap
   convertText label' = def & cellValue .~ (Just . CellText . T.pack $ label')
