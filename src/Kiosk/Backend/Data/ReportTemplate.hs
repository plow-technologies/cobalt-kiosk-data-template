{-# LANGUAGE TemplateHaskell #-}


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
-- import           Control.Applicative             ((<$>), (<*>))
import           Control.Lens

import qualified Data.Map.Lazy                   as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text                       (Text)
-- import qualified Data.Text                       as T
import           Data.Time
import           Kiosk.Backend.Data.DataTemplate
import           Kiosk.Backend.Form
import           ReportTemplate.Internal
type KioskReportTemplate context preOut rowOut= ReportTemplate context Form preOut DataTemplate rowOut


makeLenses ''Company
makeLenses ''Report
makePrisms ''ReportTable
makeLenses ''ReportTableRowStyle
makeLenses ''ReportPreamble
makePrisms ''InputType
makeLenses ''InputText
makeLenses ''InputDouble


-- | Kiosk Specific
type KioskPreambleTemplateList context preOut= [(ReportPreambleLabel, context -> Form -> preOut)]
type KioskRowTemplateList context rowOut = [(ReportRowLabel, context -> Form -> rowOut)]
type KioskPreambleTemplate context preOut= ReportPreambleTemplate context Form preOut
type KioskRowTemplate context rowOut = ReportRowTemplate context DataTemplate rowOut


-- | Spreadsheet specific

data XlsxContext = XlsxContext {
                 _xlsxCurrentTime :: UTCTime}

type XlsxReportTemplate = KioskReportTemplate XlsxContext CellMap Cell
type XlsxPreambleTemplateList = KioskRowTemplate XlsxContext CellMap
type XlsxRowTemplateList = KioskPreambleTemplate XlsxContext Cell
type XlsxReport = Report CellMap Cell
type XlsxPreamble = ReportPreamble CellMap
type XlsxTable = ReportTable Cell




 -- | Retrieve Cell DAta


makeCellDoubleFromInputDouble :: Text -> DataTemplate -> Cell
makeCellDoubleFromInputDouble = makeCellValueFromDataTemplate CellDouble inputDoubleLens
                               where
                                  inputDoubleLens = _InputTypeDouble.getInputDouble

makeCellTextFromInputText :: Text -> DataTemplate -> Cell
makeCellTextFromInputText = makeCellValueFromDataTemplate CellText inputTextLens
                                       where
                                          inputTextLens = _InputTypeText.getInputText


-- | Excel Form Rendering Helper Functions
-- Because the excel preamble is a full cell map

getCompanyName :: (Int,Int) -> Form -> CellMap
getCompanyName key form = M.insert key outputCell M.empty
  where
    outputCell :: Cell
    outputCell = def & cellValue .~ (Just . CellText $ companyName)
    companyName = form ^. getCompany.getCompanyText

-- | Row Rendering Helper Functions
makeCellValueFromDataTemplate ::
  (s -> CellValue)
  -> Getting (First s) InputType s -> Text -> DataTemplate -> Cell
makeCellValueFromDataTemplate cellConstructor  lensDt l dt  = outputCell
  where
    singletonInput = catMaybes.
                    (fmap (getItemMatchingLabel l lensDt)) .
                    templateItems $ dt

    outputCell = case singletonInput of
                   [] -> def
                   (x:_) -> def  & cellValue .~ (Just . cellConstructor $ x)

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


