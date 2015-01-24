{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell     #-}


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
                                            , fromDataTemplateEntryToS3Csv) where


import Data.Table ( Tabular
                  , Key
                  , Tab
                  , Supplemental
                  , Primary
                  , PKT
                  , fetch
                  , primary
                  , primarily
                  , mkTab
                  , forTab
                  , ixTab)
import           Control.Applicative                     ((<$>), (<*>))
import           Control.Lens                            (makeLenses, view)
import           Data.Aeson                              (ToJSON
                                                         ,FromJSON
                                                         ,Value(..)
                                                         ,parseJSON
                                                         ,toJSON
                                                         ,object
                                                         ,(.=)
                                                         ,(.:))
import           Data.ByteString.Lazy                    (ByteString)
import qualified Data.ByteString.Lazy                    as LBS
import qualified Data.List                               as L
import           Data.Text                               (Text)

import qualified Data.Csv                                as C (ToRecord, encode,
                                                               toField,
                                                               toRecord)

import qualified Data.Vector                             as V 
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
fromDataTemplateEntryToCsv templateEntries = LBS.append (appendKeyHeaders . getHeaders $ templatesWithSortedItems) (C.encode . sortDataTemplatesEntries $ templateEntries)
                           where dataTemplates  = fromDataTemplatesEntryToDataTemplates templateEntries
                                 templatesWithSortedItems = sortDataTemplatesWRemoveField <$> dataTemplates

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




-- | Tabular 

instance Tabular DataTemplateEntry where
      type PKT DataTemplateEntry  = DataTemplateEntryKey
      data Key k DataTemplateEntry b where
        Key :: Key Primary DataTemplateEntry DataTemplateEntryKey
        DValue :: Key Supplemental DataTemplateEntry DataTemplate
      data Tab DataTemplateEntry i = DTab (i Primary DataTemplateEntryKey) (i Supplemental DataTemplate)
      fetch Key = _dataTemplateEntryKey
      fetch DValue = _dataTemplateEntryValue
      primary = Key
      primarily Key r = r
      mkTab f = DTab <$> f Key <*> f DValue
      forTab (DTab i s) f = DTab <$> f Key i <*> f DValue s
      ixTab (DTab i _ ) Key = i
      ixTab (DTab _ vs) DValue = vs   
