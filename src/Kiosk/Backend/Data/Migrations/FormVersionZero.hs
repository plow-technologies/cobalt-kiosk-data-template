{-# LANGUAGE OverloadedStrings #-}


{- |
Module      :  Kiosk.Backend.Data.Migrations.FormVersionZero.hs
Description :  First Version of the Kiosk Form, 
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

-- | Form Sample
-- BBLS:_Produced_Water_1: "45"
-- Date_1: "1-26-15"
-- Driver_Signature_1: ""
-- Flowback_Water_1: ""
-- Fresh_Water_1: ""
-- Lease_Name_1: "H-2 plant"
-- Name_of_Lease_Operator_1: "Citation"
-- Name_of_Water_Hauling_Company_1: "Brady inc"
-- Pit_Water_1: ""
-- Time_in_1: ""
-- Truck_#_1: "31"
-- Water_Hauling_Permit_#_1: ""
-- signature_1: "/9j/4AAQSkZJRgABAQAASABIAAD/4QBYRXhpZgAATU0AKgAAAAgAAgESAAMAAAABAAEAAIdpAAQAAAABAAAAJgAAAAAAA6ABAAMAAAABAAEAAKACAAQAAAABAAACZ6ADAAQAAAABAAAAfQAAAAD/7QA4UGhvdG9zaG9wIDMuMAA4QklNBAQAAAAAAAA4QklNBCU
   
   

-}


module Kiosk.Backend.Data.Migrations.FormVersionZero ( FormVersionZeroEntry(..)
                                                     , FormVersionZero(..)) where

import Kiosk.Backend.Data.MigrationClass

import Data.Text (Text)
import           Kiosk.Backend.Data                (DataTemplateEntry (..),
                                                    DataTemplateEntryKey (..),
                                                    )
-- |First half of the migration



data FormVersionZeroEntry = FormVersionZeroEntry { versionZeroKey    :: DataTemplateEntryKey
                                                  , versionZeroValue :: FormVersionZero  } deriving (Show)

data FormVersionZero = FormVersionZero { _signature_1                 :: Text
                                       , _nameOfWaterHaulingCompany_1 :: Text
                                       , _flowbackWater_1             :: Text
                                       , _pitWater_1                  :: Text
                                       , _truckNumber_1               :: Text
                                       , _date_1                      :: Text
                                       , _leaseName_1                 :: Text
                                       , _waterHaulingPermit_1        :: Text
                                       , _bblsProducedWater_1         :: Text
                                       , _driverSignature_1           :: Text
                                       , _timeIn_1                    :: Text
                                       , _freshWater_1                :: Text
                                       , _nameOfLeaseOperator_1       :: Text
                                       } deriving (Show)


instance FromDataTemplate FormVersionZeroEntry where
  fromDataTemplate = toFormVersionZeroEntry



toFormVersionZeroEntry :: DataTemplateEntry -> FormVersionZeroEntry
toFormVersionZeroEntry dt = FormVersionZeroEntry dtKey formZero
                 where formZero = FormVersionZero s1 n1 fb1 p1 tn1 d1 l1 w1 b1 ds1 t1 fw1 no1
                       dtKey = extractDataTemplateEntryKey dt
                       items = extractTemplateItems dt
                       s1 = extractValueFromTemplateItems "signature_1" items
                       n1 = extractValueFromTemplateItems "Name_of_Water_Hauling_Company_1" items
                       fb1 = extractValueFromTemplateItems "Flowback_Water_1" items
                       p1 = extractValueFromTemplateItems "PitWater_1" items
                       tn1 = extractValueFromTemplateItems "Truck_#_1" items
                       d1 = extractValueFromTemplateItems "Date_1" items
                       l1 = extractValueFromTemplateItems "Lease_Name_1" items
                       w1 = extractValueFromTemplateItems "Water_Hauling_Permit_#_1" items
                       b1 = extractValueFromTemplateItems "BBLS:_Produced_Water_1" items
                       ds1 = extractValueFromTemplateItems "Driver_Signature_1" items
                       t1 = extractValueFromTemplateItems "Time_in_1" items
                       fw1 = extractValueFromTemplateItems "Fresh_Water_1" items
                       no1 = extractValueFromTemplateItems "Name_of_Lease_Operator_1" items
