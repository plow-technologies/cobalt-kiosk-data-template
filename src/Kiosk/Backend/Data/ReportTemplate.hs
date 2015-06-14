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

import           Codec.Xlsx                      (Cell (_cellValue), CellMap, CellValue (CellText, CellDouble),
                                                  Worksheet (_wsCells), def)
-- import           Control.Applicative             ((<$>), (<*>))
import           Control.Lens
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
makeLenses ''Cell




type KioskPreambleTemplateList context preOut= [(ReportPreambleLabel, context -> Form -> preOut)]

type KioskRowTemplateList context rowOut = [(ReportRowLabel, context -> Form -> rowOut)]


data XlsxContext = XlsxContext {
                 _xlsxCurrentTime :: UTCTime}


type KioskPreambleTemplate context preOut= ReportPreambleTemplate context Form preOut
type KioskRowTemplate context rowOut = ReportRowTemplate context DataTemplate rowOut

type XlsxReportTemplate = KioskReportTemplate XlsxContext CellMap Cell
type XlsxPreambleTemplateList = KioskPreambleTemplate XlsxContext CellMap
type XlsxRowTemplateList = KioskPreambleTemplate XlsxContext Cell
type XlsxPreambleTemplate = ReportPreambleTemplate XlsxContext Form CellMap
type XlsxRowTemplate  = ReportRowTemplate XlsxContext DataTemplate Cell
type XlsxPreamble = ReportPreamble CellMap
type XlsxTable = ReportTable Cell
-- Render Xlsx


renderXlsxReport xlsxRT xlsxCtx form dataTemplates = Report preambleCellMap rowCellMap
  where
     preambleCellMap = renderReportPreamble (reportPreambleTemplate xlsxRT) xlsxCtx form
     rowCellMap = renderReportRows (reportRowsTemplate xlsxRT) xlsxCtx dataTemplates
     combineCellMapGenerateWorksheet = undefined

renderReportPreamble :: XlsxPreambleTemplate -> XlsxContext -> Form -> XlsxPreamble
renderReportPreamble xlsxPreTemplate = undefined

renderReportRows :: XlsxRowTemplate -> XlsxContext -> [DataTemplate] -> XlsxTable
renderReportRows = undefined

makeCellDoubleFromInputDouble :: Text -> DataTemplate -> Cell
makeCellDoubleFromInputDouble = makeCellValueFromDataTemplate CellDouble inputDoubleLens
                               where
                                  inputDoubleLens = _InputTypeDouble.getInputDouble

makeCellTextFromInputText :: Text -> DataTemplate -> Cell
makeCellTextFromInputText = makeCellValueFromDataTemplate CellText inputTextLens
                                       where
                                          inputTextLens = _InputTypeText.getInputText

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


