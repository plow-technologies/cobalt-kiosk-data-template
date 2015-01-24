{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Kiosk.Backend.Data ( DataTemplateEntry (..)
                           ,DataTemplateEntryKey (..)
                          , DataTemplate (..)
                          , TemplateTable (..)
                          , TicketId (..)
                          , dataTemplateEntryKey
                          , dataTemplateEntryValue
                          , getTemplateTable
                          , decodeUUID
                          , getListOfSortedTemplateItems
                          , fromDataTemplateEntryToCsv
                          , fromDataTemplateEntryToS3Csv) where

-- Types
import           Data.Aeson                      (FromJSON, ToJSON,
                                                  Value (Object), object,
                                                  parseJSON, toJSON, (.:), (.=))
import           Data.UUID                       (UUID, fromString, toString)
-- Control
import           Control.Applicative             ((<$>), (<*>))

import           Control.Monad                   (liftM)

import           Data.Aeson.Types                (Parser (), Value (..))
import qualified Data.ByteString.Lazy            as LBS (append, concat,
                                                         fromStrict, length,
                                                         take)
import           Data.ByteString.Lazy.Internal   (ByteString)

import           Data.Foldable                   (toList)
import qualified Data.List                       as L (sort)


import qualified Data.Text                       as T (Text, breakOn, drop,
                                                       unpack)
import           Data.Time                       (formatTime, getCurrentTime,
                                                  getCurrentTimeZone,
                                                  utcToZonedTime)
import           Data.Time.LocalTime             (TimeZone (..))
import qualified Data.Vector                     as V (fromList, (++))

import           Plow.Extras.Time                (intToUTCTime)
import           System.Locale                   (defaultTimeLocale)










fromDataTemplatesEntryToDataTemplates :: [DataTemplateEntry] -> [DataTemplate]
fromDataTemplatesEntryToDataTemplates dtes = view dataTemplateEntryValue <$> dtes

getListOfSortedTemplateItems :: DataTemplate -> [TemplateItem]
getListOfSortedTemplateItems dts = L.sort $ templateItems dts

fromLabelsToHeaders :: [TemplateItem] -> [ByteString]
fromLabelsToHeaders tis = flip LBS.append "," <$> (LBS.fromStrict . C.toField . label <$> tis)

fromDataTemplateEntryToS3Csv :: [DataTemplateEntry] -> ByteString
fromDataTemplateEntryToS3Csv templateEntries = LBS.append (getHeaders templatesWithSortedItems) (fromDataTemplateToCSV templatesWithSortedItems)
                           where dataTemplates  = fromDataTemplatesEntryToDataTemplates templateEntries
                                 templatesWithSortedItems = sortDataTemplates <$> dataTemplates

getHeaders :: [DataTemplate] -> ByteString
getHeaders [] = ""
getHeaders lstOfTemplates = LBS.append dropComma "\r\n"
               where  bs = LBS.concat . fromLabelsToHeaders . templateItems . head $ lstOfTemplates
                      dropComma = LBS.take (LBS.length bs -1) bs

appendKeyHeaders :: ByteString -> ByteString
appendKeyHeaders = LBS.append defaultKeyHeaders

defaultKeyHeaders :: ByteString
defaultKeyHeaders = "Date,FormId,TicketId,UUID,"

sortDataTemplates :: DataTemplate -> DataTemplate
sortDataTemplates dts = dts {templateItems = newDts}
             where newDts = L.sort . filterTemplateItems $ view _templateItems dts

sortDataTemplatesWRemoveField :: DataTemplate -> DataTemplate
sortDataTemplatesWRemoveField dts = dts {templateItems = newDts}
             where newDts = L.sort . filterTemplateItems $ view _templateItems dts

sortDataTemplatesEntries :: [DataTemplateEntry] -> [DataTemplateEntry]
sortDataTemplatesEntries dtes = sortDataTemplatesEntry <$> dtes

sortDataTemplatesEntry :: DataTemplateEntry -> DataTemplateEntry
sortDataTemplatesEntry dte = dte {_dataTemplateEntryValue =s}
       where s = sortDataTemplatesWRemoveField $ view dataTemplateEntryValue dte

filterTemplateItems :: [TemplateItem] -> [TemplateItem]
filterTemplateItems = filter notSignature

notSignature :: TemplateItem -> Bool
notSignature (TemplateItem ("signature"::T.Text) (InputTypeText (InputText _))) = False
notSignature _ = True

fromDataTemplateEntryToCsv :: [DataTemplateEntry] -> ByteString
fromDataTemplateEntryToCsv templateEntries = LBS.append (appendKeyHeaders . getHeaders $ templatesWithSortedItems) (C.encode . sortDataTemplatesEntries $ templateEntries)
                           where dataTemplates  = fromDataTemplatesEntryToDataTemplates templateEntries
                                 templatesWithSortedItems = sortDataTemplatesWRemoveField <$> dataTemplates
