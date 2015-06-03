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
                                            , fromDataTemplateEntryToXlsx) where


import           Control.Applicative                     ((<$>), (<*>))
import           Control.Lens                            (makeLenses, view)
import           Data.Aeson                              (FromJSON, ToJSON,
                                                          Value (..), object,
                                                          parseJSON, toJSON,
                                                          (.:), (.=))
import           Data.ByteString.Lazy                    (ByteString)
import qualified Data.ByteString                         as BS  (ByteString)
import qualified Data.ByteString.Lazy                    as LBS
import qualified Data.List                               as L

import           Data.Text                               (Text)
import           Data.Text.Encoding                      (decodeUtf8, encodeUtf8)
import           Data.Foldable                           (foldr, foldl)

import qualified Data.Csv                                as C ((.=),
                                                               ToRecord,
                                                               encode,
                                                               toField,
                                                               toRecord,
                                                               ToRecord,
                                                               ToNamedRecord(..),
                                                               namedRecord,
                                                               toNamedRecord,
                                                               Field)

import qualified Data.Vector                             as V
import           Codec.Xlsx                              (Xlsx(..),
                                                          CellMap,
                                                          Cell(_cellValue),
                                                          CellValue(CellText),
                                                          def,
                                                          Worksheet(_wsCells))


import qualified Data.HashMap.Strict as HashMap (toList)
import           Data.Map (Map)
import qualified Data.Map as Map (empty,fromList,insert,keys,union)

import           Data.Map                                (empty, fromList, union, insert)
import qualified Data.HashMap.Strict as HM               (keys)
import           Data.Monoid                             ((<>), mempty)

import           Prelude hiding (foldr, foldl)


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
                           where dataTemplates = fromDataTemplatesEntryToDataTemplates templateEntries

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
fromDataTemplateEntryToS3Csv templateEntries = LBS.append (getHeaders templatesWithSortedItems) (fromDataTemplateToCSV templatesWithSortedItems)
                           where dataTemplates  = fromDataTemplatesEntryToDataTemplates templateEntries
                                 templatesWithSortedItems = sortDataTemplates <$> dataTemplates

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

instance C.ToNamedRecord DataTemplate where
  toNamedRecord (DataTemplate templateItems ) =
    foldl (\l -> (l <>) . C.toNamedRecord) mempty templateItems

instance C.ToNamedRecord TemplateItem where
  toNamedRecord item@TemplateItem{..} =
    C.namedRecord [ (encodeUtf8 label) C..= item ]

fromDataTemplateEntryToXlsx :: [DataTemplateEntry] -> Xlsx
fromDataTemplateEntryToXlsx dataTemplateEntries =
  fromDataTemplateEntryToXlsx' dataTemplateHeaders dataTemplateEntries
  where
    dataTemplateItems =
      concatMap (templateItems . _dataTemplateEntryValue) dataTemplateEntries

    dataTemplateHeaders =
      Map.union dataTemplateCustomHeaders dataTemplateDefaultHeaders

    dataTemplateCustomHeaders =
      foldl (\headers templateItem ->
               Map.insert (encodeUtf8 (label templateItem))
                          templateItem
                          headers)
            Map.empty
            dataTemplateItems

dataTemplateDefaultHeaders :: Map BS.ByteString TemplateItem
dataTemplateDefaultHeaders =
  Map.fromList [("UUID",    headerTemplateItem "UUID")
               ,("TicketId",headerTemplateItem "TicketId")
               ,("FormId",  headerTemplateItem "FormId")
               ,("Date",    headerTemplateItem "Date")]
  where
    headerTemplateItem header = TemplateItem
      { label         = header
      , templateValue = InputTypeText (InputText header)
      }

fromDataTemplateEntryToXlsx' :: (C.ToRecord a, C.ToNamedRecord b) => b -> [a] -> Xlsx
fromDataTemplateEntryToXlsx' headers_ data_ = def { _xlSheets = workSheets }
  where
    workSheets = fromList [("", workSheet)]
    workSheet  = def { _wsCells = headerCells `union` dataCells }
    dataCells  = snd $ foldr mkCellsFromRecord (dataCellsStartRow, empty) data_
    headerCells = mkHeaderCells headers_
    dataCellsStartRow = 1

mkHeaderCells :: C.ToNamedRecord b => b -> CellMap
mkHeaderCells b = fst $ foldl fn (mempty, 0) names
   where
    names = Map.keys (Map.fromList (HashMap.toList (C.toNamedRecord b)))

fn :: (CellMap, ColumnIndex) -> BS.ByteString -> (CellMap, ColumnIndex)
fn (cellMap, col) name = (cellMap', col + 1)
  where
    cell     = def{ _cellValue = Just (CellText (decodeUtf8 name)) }
    cellMap' = insert (0,col) cell cellMap

mkCellsFromRecord :: C.ToRecord a => a
                  -> (RowIndex, Row)
                  -> (RowIndex, Row)
mkCellsFromRecord a (rowIndex, acc) = (rowIndex + 1, union acc row)
 where
  (_, row) = foldr rowFromField
                       (0, empty)
                       (C.toRecord a)
  rowFromField field (columnIndex, acc') =
    mkCellFromFields rowIndex columnIndex field acc'

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
