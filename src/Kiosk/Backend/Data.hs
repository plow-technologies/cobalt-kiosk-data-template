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
                          , TemplateItem (..)
                          , dataTemplateEntryKey
                          , dataTemplateEntryValue
                          , getTemplateTable
                          , decodeUUID
                          , getListOfSortedTemplateItems
                          , fromDataTemplateEntryToCsv
                          , fromDataTemplateEntryToS3Csv) where

import Kiosk.Backend.Table

import Kiosk.Backend.Data.DataTemplate
import Kiosk.Backend.Data.DataTemplateEntry
import Kiosk.Backend.Data.DataTemplateEntryKey








