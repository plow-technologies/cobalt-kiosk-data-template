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


module Kiosk.Backend.Data.Migrations (FormVersionZeroEntry(..), FormVersionOneEntry (..)) where

import           Control.Lens                      (view)
import           Data.Text
import           Kiosk.Backend.Data                (DataTemplateEntry (..),
                                                    DataTemplateEntryKey (..),
                                                    dataTemplateEntryKey,
                                                    dataTemplateEntryValue)
import           Kiosk.Backend.Data.DataTemplate   (DataTemplate (..),
                                                    TemplateItem (..))
import           Kiosk.Backend.Data.MigrationClass

import qualified Data.List                         as L (find)
import           Kiosk.Backend.Form.Element


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
                                       , nameOfLeaseOperator_1       :: Text
                                       }
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

toFormVersionZeroEntry :: DataTemplateEntry -> FormVersionZeroEntry
toFormVersionZeroEntry dt = FormVersionZeroEntry dtKey formZero
                 where formZero = FormVersionZero s1 n1 fb1 p1 tn1 d1 l1 w1 b1 ds1 t1 fw1 no1
                       dtKey = extractDataTemplateEntryKey dt
                       items = extractTemplateItems dt
                       s1 = extractValueFromTemplateItem "signature_1" items
                       n1 = extractValueFromTemplateItem "Name_of_Water_Hauling_Company_1" items
                       fb1 = extractValueFromTemplateItem "Flowback_Water_1" items
                       p1 = extractValueFromTemplateItem "PitWater_1" items
                       tn1 = extractValueFromTemplateItem "Truck_#_1" items
                       d1 = extractValueFromTemplateItem "Date_1" items
                       l1 = extractValueFromTemplateItem "Lease_Name_1" items
                       w1 = extractValueFromTemplateItem "Water_Hauling_Permit_#_1" items
                       b1 = extractValueFromTemplateItem "BBLS:_Produced_Water_1" items
                       ds1 = extractValueFromTemplateItem "Driver_Signature_1" items
                       t1 = extractValueFromTemplateItem "Time_in_1" items
                       fw1 = extractValueFromTemplateItem "Fresh_Water_1" items
                       no1 = extractValueFromTemplateItem "Name_of_Lease_Operator_1" items


extractDataTemplateEntryKey :: DataTemplateEntry -> DataTemplateEntryKey
extractDataTemplateEntryKey = view dataTemplateEntryKey

extractTemplateItems :: DataTemplateEntry -> [TemplateItem]
extractTemplateItems dt  = templateItems $ view dataTemplateEntryValue dt

extractValueFromTemplateItem :: String -> [TemplateItem] -> Text
extractValueFromTemplateItem key items = case L.find (\a -> label a == pack key) items of
                                                  Just item -> convertInputTypeToValue item
                                                  Nothing -> ("Error"::Text)

convertInputTypeToValue = extractInputType

extractInputType :: TemplateItem -> Text
extractInputType (TemplateItem _ (InputTypeText (InputText t))) = t
extractInputType (TemplateItem _ (InputTypeInt (InputInt i))) = pack . show $ i
extractInputType (TemplateItem _ (InputTypeDouble (InputDouble d))) = pack . show $ d
extractInputType (TemplateItem _ (InputTypeSignature (Signature s))) = s

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

data WaterType = PitWater | FlowBackWater | FreshWater | ProducedWater deriving (Show)

data FormVersionOneEntry = FormVersionOneEntry { versionOneKey   :: DataTemplateEntryKey
                                               , versionOneValue :: FormVersionOne  }


data FormVersionOne = FormVersionOne { nameOfWaterHaulingCompany ::Text
                                     , amount                    :: Int
                                     , date                      :: Text
                                     , timeIn                    :: Text
                                     , typeOfWaterHauled         :: WaterType
                                     , truckNumber               :: Text
                                     , waterHaulingPermit        :: Text
                                     , nameOfLeaseOperator       :: Text
                                     , leaseName                 :: Text
                                     , signature                 :: Text  }

-- | FromVersionOne Sample
-- [[key: {
--     date = 1421854997047;
--     entryid = "142185499704709CDCCC3-7DC1-4732-BA80-47F635BDDB641";
--     formid = 1;
--     ticketid = "0009-000002";
--     uuid = "09CDCCC3-7DC1-4732-BA80-47F635BDDB64";
-- }, value: {
--     Amount = 100;
--     Date = "01/21/2015";
--     "Driver_Signature" = "";
--     "Lease_Name" = "Big Oil Field";
--     "Name_of_Lease_Operator" = James;
--     "Name_of_Water_Hauling_Company" = 1234;
--     "Time_in" = "11:42 PM";
--     "Truck_#" = abcd;
--     "Type_of_Water_Hauled" = "Flowback Water";
--     "Water_Hauling_Permit_#" = 7890;
-- }]]


fromFormVersionOne :: FormVersionOneEntry -> DataTemplateEntry
fromFormVersionOne f1e = DataTemplateEntry dtKey dtValue
                 where dtKey = versionOneKey f1e
                       dtValue = convertFormVersionOneToDataTemplate (versionOneValue f1e)

convertFormVersionOneToDataTemplate ::  FormVersionOne -> DataTemplate
convertFormVersionOneToDataTemplate fv1 = dataTemplateBuilder fv1

dataTemplateBuilder :: FormVersionOne -> DataTemplate
dataTemplateBuilder fv1 = DataTemplate $ convertFormToTemplateItems fv1

printWaterType :: WaterType -> String
printWaterType wt = case wt of
           PitWater      -> "Pit Water"
           FlowBackWater -> "Flow Back Water"
           FreshWater    -> "Fresh Water"
           ProducedWater -> "Produced Water"


convertFormToTemplateItems fv1 = [nItem, a , d ,tin , twh, tn, nlo, ln, s ]
                        where nItem = makeTemplateItemText "Name_of_Water_Hauling_Company" (nameOfWaterHaulingCompany fv1)
                              a     = makeTemplateItemInt "Amount" (amount fv1)
                              d     = makeTemplateItemText "Date" (date fv1)
                              tin   = makeTemplateItemText "Time_in" (timeIn fv1)
                              twh   = makeTemplateItemText "Type_of_Water_Hauled" (pack . printWaterType . typeOfWaterHauled $ fv1)
                              tn    = makeTemplateItemText "Water_Hauling_Permit#" (waterHaulingPermit fv1)
                              nlo   = makeTemplateItemText "Name_of_Lease_Operator" (nameOfLeaseOperator fv1)
                              ln    = makeTemplateItemText "Lease_Name" (leaseName fv1)
                              s     = makeTemplateItemText "Driver_Signature" (signature fv1)


makeTemplateItemInt :: String -> Int -> TemplateItem
makeTemplateItemInt label value = TemplateItem (pack label) (InputTypeInt (InputInt value))

makeTemplateItemText :: String -> Text -> TemplateItem
makeTemplateItemText label value = TemplateItem (pack label) (InputTypeText (InputText value))


formVersionZeroEntryToFormOneEntry :: FormVersionZeroEntry -> FormVersionOneEntry
formVersionZeroEntryToFormOneEntry fv0 = FormVersionOneEntry fv1Key fv1Value
                            where fv1Key = versionZeroKey fv0
                                  fv1Value = formVersionZeroToFormVersionOne $ versionZeroValue fv0

formVersionZeroToFormVersionOne (FormVersionZero s1 n1 fbw1 pw1 tn1 d1 ln1 wp1 bpw1 ds1 ti1 fw1 nl1) = FormVersionOne n1 (read (unpack bpw1)::Int) d1 ti1 FreshWater tn1 wp1 nl1 ln1 s1


 -- { signature_1                 :: Text
 --                                       , nameOfWaterHaulingCompany_1 ::Text
 --                                       , flowbackWater_1             :: Text
 --                                       , pitWater_1                  :: Text
 --                                       , truckNumber_1               :: Text
 --                                       , date_1                      ::Text
 --                                       , leaseName_1                 :: Text
 --                                       , waterHaulingPermit_1        :: Text
 --                                       , bblsProducedWater_1         :: Text
 --                                       , driverSignature_1           :: Text
 --                                       , timeIn_1                    :: Text
 --                                       , freshWater_1                :: Text
 --                                       , nameOfLeaseOperator_1       :: Text
 --                                       }

-- data FormVersionOne = FormVersionOne { nameOfWaterHaulingCompany ::Text
--                                      , amount                    :: Int
--                                      , date                      :: Text
--                                      , timeIn                    :: Text
--                                      , typeOfWaterHauled         :: WaterType
--                                      , truckNumber               :: Text
--                                      , waterHaulingPermit        :: Text
--                                      , nameOfLeaseOperator       :: Text
--                                      , leaseName                 :: Text
--                                      , signature                 :: Text  }
