
{-# LANGUAGE OverloadedStrings #-}



{- |
Module      :  Kiosk.Backend.Data.Migrations
Description :  Every known migration between templates
Copyright   :  Plow Technologies LLC
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

Migrations are one off and case by case, therefore this file is expected to grow.
Each migration is tied to a form-id and another form-id.  Then there is a parser that
     takes one to another.

     There is nothing else that is required


-}


module Kiosk.Backend.Data.Migrations (FormVersionZeroEntry(..)) where

import           Control.Lens                      (view)
import           Data.Text
import           Kiosk.Backend.Data                (DataTemplateEntry (..),
                                                    DataTemplateEntryKey (..),
                                                    dataTemplateEntryKey,
                                                    dataTemplateEntryValue)
import           Kiosk.Backend.Data.DataTemplate   (DataTemplate (..))
import           Kiosk.Backend.Data.MigrationClass



-- | MigrationClasses are about migrating data not data templates so worry about only the output


-- |First half of the migration
data FormVersionZeroEntry = FormVersionZeroEntry { versionZeroKey    :: DataTemplateEntryKey
                                                  , versionZeroValue :: FormVersionZero  }

data FormVersionZero = FormVersionZero { signature_1                 :: Text
                                       , nameOfWaterHaulingCompany_1 ::Text
                                       , flowbackWater_1             :: Text
                                       , pitWater_1                  :: Text
                                       , truckNumber_1               :: Text
                                       , date_1                      ::Text
                                       , leaseName_1                 :: Text
                                       , waterHaulingPermit_1        :: Text
                                       , bblsProducedWater_1         :: Text
                                       , driverSignature_1           :: Text
                                       , timeIn_1                    :: Text
                                       , freshWater_1                :: Text
                                       , nameOfLeaseOperator_1       :: Text  }

toFormVersionZeroEntry :: DataTemplateEntry -> FormVersionZeroEntry
toFormVersionZeroEntry = undefined


extractDataTemplateEntryKey :: DataTemplateEntry -> DataTemplateEntryKey
extractDataTemplateEntryKey = view dataTemplateEntryKey

extractTemplateItems :: DataTemplateEntry -> [TemplateItem]
extractTemplateItems dt  = templateItems $ view dataTemplateValue dt

extractValueFromTemplateItem :: String -> [TemplateItem] -> Text
extractValueFromTemplateItem key items = undefined

-- | Second half of the migration

{-
[
  {
    "value": {
      "Time_in": "6:16 PM",
      "Date": "01\/23\/2015",
      "Name_of_Water_Hauling_Company": "C and J Oil Field Service",
      "Amount": "3333",
      "Type_of_Water_Hauled": "Fresh Water",
      "Truck_#": "384",
      "Water_Hauling_Permit_#": "Jfjs",
      "Name_of_Lease_Operator": "Ijnd",
      "Lease_Name": "Jjji"
    },
    "key": {
      "date": "1422059058389",
      "ticketid": "0013-000001",
      "formid": "1",
      "uuid": "54B6AF93-08F2-42AD-AA28-8C0565FF4E2C",
      "entryid": "142205905838954B6AF93-08F2-42AD-AA28-8C0565FF4E2C1"
    }
  }
]

 -}

data WaterType = FreshWater

data FormVersionOneEntry = FormVersionOneEntry { versionOneKey   :: DataTemplateEntryKey
                                               , versionOneValue :: FormVersionOne  }


data FormVersionOne = FormVersionOne { nameOfWaterHaulingCompany ::Text
                                     , amount                    :: Int
                                     , typeOfWaterHauled         :: WaterType
                                     , truckNumber               :: Text
                                     , waterHaulingPermit        :: Text
                                     , nameOfLeaseOperator       :: Text
                                     , leaseName                 :: Text
                                     , signature                 :: Text  }


fromFormVersionOne :: FormVersionOneEntry -> DataTemplateEntry
fromFormVersionOne = undefined
