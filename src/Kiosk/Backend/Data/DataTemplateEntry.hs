{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RecordWildCards     #-}

{- |
Module      :  Kiosk.Backend.Data.DataTemplateEntry
Description :  Individual Entries for Data Templates
Copyright   :  Plow Technologies LLC
License     :  MIT License

Maintainer  :
Stability   :  experimental
Portability :  portable

description

-}

module Kiosk.Backend.Data.DataTemplateEntry ( DataTemplateEntry(..)
                                            , dataTemplateEntryKey
                                            , dataTemplateEntryValue
                                            , getListOfSortedTemplateItems
                                            , fromDataTemplateEntryToCsv
                                            , fromDataTemplateEntryToS3Csv
                                            , fromDataTemplateEntryToXlsxWorksheet  
                                            ) where


import           Control.Applicative                     ((<$>), (<*>))
import           Control.Lens                            (makeLenses, view)
import           Data.Aeson                              (FromJSON, ToJSON,
                                                          Value (..), object,
                                                          parseJSON, toJSON,
                                                          (.:), (.=))
import           Data.ByteString.Lazy                    (ByteString)
--import qualified Data.ByteString                         as BS  (ByteString)
import qualified Data.ByteString.Lazy                    as LBS
import qualified Data.List                               as L

import           Data.Text                               (Text)
import           Data.Text.Encoding                      (decodeUtf8)
import           Data.Foldable                           (foldr, foldl)

import qualified Data.Csv                                as C (ToRecord,
                                                               encode,
                                                               toField,
                                                               toRecord,
                                                               ToRecord,
                                                               Field)

import qualified Data.Vector                             as V
import           Codec.Xlsx                              (CellMap,
                                                          Cell(_cellValue),
                                                          CellValue(CellText),
                                                          def,
                                                          Worksheet(_wsCells),
                                                         )
import           Data.Map                                (empty, union, insert)
import           Data.Monoid                             (mempty)
import           Prelude hiding                          (foldr, foldl)

import           Kiosk.Backend.Data.DataTemplate         (DataTemplate (..),
                                                          InputText (..),
                                                          InputType (..),
                                                          TemplateItem (..),
                                                          fromDataTemplateToCSV,
                                                          _templateItems)
import           Kiosk.Backend.Data.DataTemplateEntryKey


-- |Data Template Entry defines a return value of a form
data DataTemplateEntry = DataTemplateEntry {
                       _dataTemplateEntryKey   :: DataTemplateEntryKey,
                       _dataTemplateEntryValue :: DataTemplate
                                             }
          deriving (Show,Eq,Ord)


instance C.ToRecord DataTemplateEntry where
  toRecord (DataTemplateEntry dtk dtv) = C.toRecord dtk V.++ C.toRecord dtv

makeLenses ''DataTemplateEntry

-- | Query Helpers
getListOfSortedTemplateItems :: DataTemplate -> [TemplateItem]
getListOfSortedTemplateItems dts = L.sort $ templateItems dts


-- | Aeson Instances


instance ToJSON DataTemplateEntry where
  toJSON (DataTemplateEntry k v) = object ["key" .= k
                                          ,"value" .= v]

instance FromJSON DataTemplateEntry where
  parseJSON (Object o) = DataTemplateEntry <$> o .: "key"
                                           <*> o .: "value"
  parseJSON _          = fail "Expecting DateTemplateEntry object, Received Other"


-- | CSV Stuff

fromDataTemplateEntryToCsv :: [DataTemplateEntry] -> ByteString
fromDataTemplateEntryToCsv templateEntries = LBS.append (appendKeyHeaders . getHeaders $ templatesWithSortedItems templateEntries)
                                                        (C.encode . sortDataTemplatesEntries $ templateEntries)

templatesWithSortedItems :: [DataTemplateEntry] -> [DataTemplate]
templatesWithSortedItems dataTemplateEntries =
  sortDataTemplatesWRemoveField `fmap`
  (fromDataTemplatesEntryToDataTemplates dataTemplateEntries)

sortDataTemplatesWRemoveField :: DataTemplate -> DataTemplate
sortDataTemplatesWRemoveField dts = dts {templateItems = newDts}
             where newDts = L.sort . filterTemplateItems $ view _templateItems dts

filterTemplateItems :: [TemplateItem] -> [TemplateItem]
filterTemplateItems = filter notSignature

-- | Remove the signature from a CSV file
notSignature :: TemplateItem -> Bool
notSignature (TemplateItem ("signature"::Text) (InputTypeText (InputText _))) = False
notSignature _ = True

defaultKeyHeaders :: ByteString
defaultKeyHeaders = "Date,FormId,TicketId,UUID,"

appendKeyHeaders :: ByteString -> ByteString
appendKeyHeaders = LBS.append defaultKeyHeaders

-- |Header Creation
getHeaders :: [DataTemplate] -> ByteString
getHeaders [] = ""
getHeaders lstOfTemplates = LBS.append dropComma "\r\n"
               where  bs = LBS.concat . fromLabelsToHeaders . templateItems . head $ lstOfTemplates
                      dropComma = LBS.take (LBS.length bs -1) bs

-- | Transformations
fromDataTemplateEntryToS3Csv :: [DataTemplateEntry] -> ByteString
fromDataTemplateEntryToS3Csv templateEntries = LBS.append (getHeaders templatesWithSortedItems_) (fromDataTemplateToCSV templatesWithSortedItems_)
                           where dataTemplates  = fromDataTemplatesEntryToDataTemplates templateEntries
                                 templatesWithSortedItems_ = sortDataTemplates <$> dataTemplates

fromDataTemplatesEntryToDataTemplates :: [DataTemplateEntry] -> [DataTemplate]
fromDataTemplatesEntryToDataTemplates dtes = view dataTemplateEntryValue <$> dtes

fromLabelsToHeaders :: [TemplateItem] -> [ByteString]
fromLabelsToHeaders tis = flip LBS.append "," <$> (LBS.fromStrict . C.toField . label <$> tis)

-- | Ordering

sortDataTemplates :: DataTemplate -> DataTemplate
sortDataTemplates dts = dts {templateItems = newDts}
             where newDts = L.sort . filterTemplateItems $ view _templateItems dts

sortDataTemplatesEntries :: [DataTemplateEntry] -> [DataTemplateEntry]
sortDataTemplatesEntries dtes = sortDataTemplatesEntry <$> dtes

sortDataTemplatesEntry :: DataTemplateEntry -> DataTemplateEntry
sortDataTemplatesEntry dte = dte {_dataTemplateEntryValue =s}
       where s = sortDataTemplatesWRemoveField $ view dataTemplateEntryValue dte

type RowIndex    = Int
type ColumnIndex = Int
type Row         = CellMap

type Header = Text

fromDataTemplateEntryToXlsxWorksheet :: [DataTemplateEntry] -> Worksheet
fromDataTemplateEntryToXlsxWorksheet []                  = def
fromDataTemplateEntryToXlsxWorksheet dataTemplateEntries =
  fromDataTemplateEntryToXlsx' dataTemplateHeaders sortedDataTemplateEntries
  where
    sortedDataTemplateEntries = sortDataTemplatesEntries dataTemplateEntries

    dataTemplateItems =
      head (map (templateItems . _dataTemplateEntryValue) sortedDataTemplateEntries)

    dataTemplateHeaders = dataTemplateDefaultHeaders ++ dataTemplateCustomHeaders

    dataTemplateCustomHeaders = map label dataTemplateItems

dataTemplateDefaultHeaders :: [Text]
dataTemplateDefaultHeaders = ["Date","FormId","TicketId","UUID"]

fromDataTemplateEntryToXlsx' :: [Text] -> [DataTemplateEntry] -> Worksheet
fromDataTemplateEntryToXlsx' headers_ data_ = def { _wsCells = headerCells `union` dataCells }
  where
    dataCells  = snd $ foldr (mkCellsFromRecord columnIndexes) (dataCellsStartRow, empty) data_
    headerCells = mkHeaderCells headers_
    dataCellsStartRow = 2

    columnIndexes = zip headers_ [1..]

mkHeaderCells :: [Text] -> CellMap
mkHeaderCells names = fst $ foldl fn (mempty, 1) names

fn :: (CellMap, ColumnIndex) -> Text -> (CellMap, ColumnIndex)
fn (cellMap, col) name = (cellMap', col + 1)
  where
    cell     = def{ _cellValue = Just (CellText name) }
    cellMap' = insert (1,col) cell cellMap

mkCellsFromRecord :: [(Header,ColumnIndex)]
                  -> DataTemplateEntry
                  -> (RowIndex, Row)
                  -> (RowIndex, Row)
mkCellsFromRecord columnIndexes a (rowIndex, acc) =(rowIndex + 1,union acc row)
  where
    row = union keyRow valueRow

    (_, keyRow) = foldl rowFromField
                        (1, empty)
                        (C.toRecord (_dataTemplateEntryKey a))
    rowFromField (columnIndex, acc') field =
      mkCellFromFields rowIndex columnIndex field acc'


    valueItems = templateItems (_dataTemplateEntryValue a)
    valueRow   = makeRowFromTemplateItems rowIndex columnIndexes valueItems

mkCellFromFields :: RowIndex
                 -> ColumnIndex
                 -> C.Field
                 -> Row
                 -> (ColumnIndex, Row)
mkCellFromFields rowIndex
                 columnIndex
                 field
                 row =
  (columnIndex + 1, insert (rowIndex,columnIndex) cell row)
 where
   cell      = def { _cellValue = Just (CellText cellValue) }
   cellValue = decodeUtf8 field

makeRowFromTemplateItems
  :: RowIndex
  -> [(Header,ColumnIndex)]
  -> [TemplateItem]
  -> Row
makeRowFromTemplateItems rowIndex columnIndexes =
  foldl (flip (insertTemplateItemInRow rowIndex columnIndexes)) empty

insertTemplateItemInRow
  :: RowIndex
  -> [(Header,ColumnIndex)]
  -> TemplateItem
  -> Row
  -> Row
insertTemplateItemInRow rowIndex columnIndexes templateItem row =
  case lookup header columnIndexes of
    Nothing          -> row
    Just columnIndex -> insert (rowIndex,columnIndex) cell row
  where
    header    = label templateItem
    cell      = def { _cellValue = Just (CellText cellValue) }
    cellValue = decodeUtf8 (C.toField templateItem)
