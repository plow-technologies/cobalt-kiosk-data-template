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












