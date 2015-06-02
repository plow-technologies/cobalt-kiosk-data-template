{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Data.DataTemplateEntrySpec where

-- import           Control.Applicative             ((<$>))
-- import           Control.Arrow                   ((***))
-- import           Data.Aeson                      (Value (..), decode,
--                                                   eitherDecode, encode, toJSON)
-- import           Data.ByteString.Lazy.Internal   (ByteString)
-- import           Data.Either                     (rights)

-- import qualified Data.HashMap.Strict             as HM
-- import           Data.List                       (sort)
-- import           Data.Text                       (Text)
-- import           Generators                      (GeneratorType (..), checkStaticGeneratorConsistency,
--                                                   generateDataTemplateEntry,
--                                                   generateForm)

import           Codec.Xlsx.Types                ( def
                                                 , Xlsx (..)
                                                 , Worksheet (..)
                                                 )
import           Data.Maybe                      (fromMaybe)
import           Data.List                       (nub)
import qualified Data.Map as M                   (lookup, keys, size)
import           Kiosk.Backend.Data.DataTemplateEntry (fromDataTemplateEntryToXlsx)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck                 -- (quickCheckWith,stdArgs)
import           Generators

import           Kiosk.Backend.Data              (DataTemplateEntry (..),DataTemplate(..))

import Data.Map (toList)
import Data.Maybe (fromJust)
import Data.UUID (fromString)
import Kiosk.Backend.Data
import Kiosk.Backend.Form


-- import           TestImport                      (testJSON)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  modifyMaxSuccess (\_ -> 100) . modifyMaxSize (\_ -> 10) $
    describe "no data loss" $ do
      it "xlsx should have a row for each data template entry" $
        property prop_no_data_loss_rows
      it "xlsx should have a cell for each field" $
        property prop_no_data_loss_cells

prop_no_data_loss_rows :: [DataTemplateEntry] -> Bool
prop_no_data_loss_rows xs = numDataTemplateEntries == numRowsInExcelSheet
 where
  numDataTemplateEntries = length xs + numHeaderRows
  sheet                  = fromMaybe def $ M.lookup "" (_xlSheets (fromDataTemplateEntryToXlsx xs))
  numRowsInExcelSheet    = length . nub . (fmap fst) . M.keys  $  _wsCells sheet
  numHeaderRows          = 1
  
prop_no_data_loss_cells :: [DataTemplateEntry] -> Bool
prop_no_data_loss_cells xs = numCellsInDataTemplateEntries == numCellsInExcelSheet
  where
    numCellsInDataTemplateEntries =
      sum (map (length . templateItems . _dataTemplateEntryValue) xs) +
      numDefaultKeyHeaders * length xs
    numDefaultKeyHeaders = 4
    sheet = fromMaybe def $ M.lookup "" (_xlSheets (fromDataTemplateEntryToXlsx xs))
    numCellsInExcelSheet = M.size (_wsCells sheet)

numCellsInDataTemplateEntries xs =
  sum (map (length . templateItems . _dataTemplateEntryValue) xs) + length xs

sheet xs = fromMaybe def $ M.lookup "" (_xlSheets (fromDataTemplateEntryToXlsx xs))

numCellsInExcelSheet xs = M.size (_wsCells $ sheet xs)

testItems = concatMap (templateItems . _dataTemplateEntryValue) testDataTemplateEntries
testCells = toList (_wsCells (sheet testDataTemplateEntries))

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
      [TemplateItem "1b" (InputTypeText (InputText "A"))
      ,TemplateItem "2b" (InputTypeText (InputText "B"))
      ,TemplateItem "3b" (InputTypeText (InputText "C"))
      ,TemplateItem "4b" (InputTypeText (InputText "D"))
      ,TemplateItem "5b" (InputTypeText (InputText "E"))]
  }
