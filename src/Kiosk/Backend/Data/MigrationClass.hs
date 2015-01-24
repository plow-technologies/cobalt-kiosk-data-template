{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGEltiParamTypeClasses #-}
{-# LANGUAGEltiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      :  Kiosk.Backend.Data.MigrationClass
Description :  Migrations from one form to another
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

Migrations take one form entry to another,
They are ad-hock but the form is set,
forms start out without a structure and are sent through 
an intermediate type with a MigrationClass created on it,
this results in a final type   

-}
module Kiosk.Backend.Data.MigrationClass ( FormMigration
                                         , IncomingRecord
                                         , OutgoingRecord
                                         , toIncomingRecord
                                         , transformRecord 
                                         , fromOutgoingRecord) where

import Kiosk.Backend.Data ( DataTemplateEntry(..)
                          , DataTemplateEntryKey(..))
                          

class FormMigration i o where 
  type IncomingRecord i :: * 
  type OutgoingRecord o :: *
  toIncomingRecord :: DataTemplateEntry -> IncomingRecord i
  transformRecord :: IncomingRecord i -> OutgoingRecord o
  fromOutgoingRecord :: OutgoingRecord o -> DataTemplateEntry










