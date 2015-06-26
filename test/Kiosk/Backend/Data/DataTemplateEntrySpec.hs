{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Data.DataTemplateEntrySpec (spec) where

import Generators            ()
import Kiosk.Backend.Data    (DataTemplate(..)
                             ,DataTemplateEntry(..)
                             ,fromDataTemplateEntryToXlsxWorksheet)

import Codec.Xlsx.Types      (Worksheet(..))
import Data.List             (nub)
import Data.Map              (keys)
import Test.Hspec            (describe,it,Spec)
import Test.Hspec.QuickCheck (modifyMaxSize,modifyMaxSuccess)
import Test.QuickCheck       (property)

spec :: Spec
spec = dataTemplateEntrySpec

dataTemplateEntrySpec :: Spec
dataTemplateEntrySpec = do
  modifyMaxSuccess (\_ -> 100) . modifyMaxSize (\_ -> 10) $
    describe "no data loss" $ do
      it "xlsx should have a row for each data template entry"
         (property prop_noDataLossRows)

      it "xlsx should have a column for each header"
         (property prop_noDataLossColumns)

prop_noDataLossRows :: [DataTemplateEntry] -> Bool
prop_noDataLossRows xs = numDataTemplateEntries == numRowsInExcelSheet
 where
  numDataTemplateEntries = length xs + numHeaderRows
  sheet_                 = fromDataTemplateEntryToXlsxWorksheet xs
  numRowsInExcelSheet    = length . nub . (fmap fst) . keys  $  _wsCells sheet_
  numHeaderRows          = if null xs then 0 else 1

prop_noDataLossColumns :: [DataTemplateEntry] -> Bool
prop_noDataLossColumns dataTemplateEntries =
  numColumnsInDataTemplateEntries == numColumnsInWorksheet
  where
    numColumnsInDataTemplateEntries =
      if null dataTemplateEntries
         then 0
         else numHeadersInDataTemplateEntries + numDefaultHeaders

    numHeadersInDataTemplateEntries =
      maximum (fmap (length . templateItems . _dataTemplateEntryValue) dataTemplateEntries)

    numDefaultHeaders = 4

    worksheet = fromDataTemplateEntryToXlsxWorksheet dataTemplateEntries

    numColumnsInWorksheet =
      length (nub (fmap snd (keys (_wsCells worksheet))))
