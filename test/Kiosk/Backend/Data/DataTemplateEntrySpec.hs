{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Data.DataTemplateEntrySpec where

import           Codec.Xlsx.Types                     (Worksheet(..))
import           Data.List                            (nub)
import qualified Data.Map                             as M (keys, size)
import           Data.Maybe                           (fromJust)
import           Data.UUID                            (fromString)
import           Generators                           ()
import           Kiosk.Backend.Data.DataTemplateEntry (fromDataTemplateEntryToXlsxWorksheet)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck                      (property)

import           Kiosk.Backend.Data hiding (fromDataTemplateEntryToXlsxWorksheet)
import           Kiosk.Backend.Form

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  modifyMaxSuccess (\_ -> 100) . modifyMaxSize (\_ -> 10) $
    describe "no data loss" $ do
      it "xlsx should have a row for each data template entry" $ property prop_no_data_loss_rows
--      it "xlsx should have a cell for each field"              $ property prop_no_data_loss_cells

prop_no_data_loss_rows :: [DataTemplateEntry] -> Bool
prop_no_data_loss_rows xs = numDataTemplateEntries == numRowsInExcelSheet
 where
  numDataTemplateEntries = length xs + numHeaderRows
  sheet_                 = fromDataTemplateEntryToXlsxWorksheet xs
  numRowsInExcelSheet    = length . nub . (fmap fst) . M.keys  $  _wsCells sheet_
  numHeaderRows          = if null xs then 0 else 1

prop_no_data_loss_cells :: [DataTemplateEntry] -> Bool
prop_no_data_loss_cells xs = numCellsInDataTemplateEntries_ == numCellsInExcelSheet_
  where
    numCellsInDataTemplateEntries_ =
      if null xs
         then 0
         else numHeaders + numCells

    numHeaders =
      maximum (fmap (length . templateItems . _dataTemplateEntryValue) xs)
        + numDefaultKeyHeaders

    numCells =
      sum (fmap ((+) numDefaultKeyHeaders . length . templateItems . _dataTemplateEntryValue) xs)

    numDefaultKeyHeaders = 4
    sheet_ = fromDataTemplateEntryToXlsxWorksheet xs
    numCellsInExcelSheet_ = M.size (_wsCells sheet_)

numCellsInDataTemplateEntries :: [DataTemplateEntry] -> Int
numCellsInDataTemplateEntries xs =
  sum (map (length . templateItems . _dataTemplateEntryValue) xs) + length xs

sheet :: [DataTemplateEntry] -> Worksheet
sheet = fromDataTemplateEntryToXlsxWorksheet

numCellsInExcelSheet :: [DataTemplateEntry] -> Int
numCellsInExcelSheet xs = M.size (_wsCells $ sheet xs)

testDataTemplateEntries :: [DataTemplateEntry]
testDataTemplateEntries = [testDataTemplateEntry1,testDataTemplateEntry2]

testDataTemplateEntry1,testDataTemplateEntry2 :: DataTemplateEntry

testDataTemplateEntry1 = DataTemplateEntry
  { _dataTemplateEntryKey   = testDataTemplateEntryKey1
  , _dataTemplateEntryValue = testDataTemplateEntryValue1
  }

testDataTemplateEntry2 = DataTemplateEntry
  { _dataTemplateEntryKey   = testDataTemplateEntryKey2
  , _dataTemplateEntryValue = testDataTemplateEntryValue2
  }

testDataTemplateEntryKey1,testDataTemplateEntryKey2 :: DataTemplateEntryKey

testDataTemplateEntryKey1 = DataTemplateEntryKey
  { _getDate     = 1
  , _getUUID     = fromJust (fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c")
  , _getTicketId = TicketId (1,1)
  , _getFormId   = 1
  }

testDataTemplateEntryKey2 = DataTemplateEntryKey
  { _getDate     = 2
  , _getUUID     = fromJust (fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c")
  , _getTicketId = TicketId (2,2)
  , _getFormId   = 2
  }

testDataTemplateEntryValue1,testDataTemplateEntryValue2 :: DataTemplate

testDataTemplateEntryValue1 = DataTemplate
  { templateItems =
      [TemplateItem "1a" (InputTypeText (InputText "A"))
      ,TemplateItem "2a" (InputTypeText (InputText "B"))
      ,TemplateItem "3a" (InputTypeText (InputText "C"))
      ,TemplateItem "4a" (InputTypeText (InputText "D"))
      ,TemplateItem "5a" (InputTypeText (InputText "E"))]
  }

testDataTemplateEntryValue2 = DataTemplate
  { templateItems =
      [TemplateItem "1a" (InputTypeText (InputText "A"))
      ,TemplateItem "2a" (InputTypeText (InputText "B"))
      ,TemplateItem "3a" (InputTypeText (InputText "C"))
 --     ,TemplateItem "4a" (InputTypeText (InputText "D"))
      ,TemplateItem "5a" (InputTypeText (InputText "E"))]
  }
