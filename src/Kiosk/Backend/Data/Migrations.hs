
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

import Kiosk.Backend.Data.MigrationClass 
import Data.Text
import Kiosk.Backend.Data ( DataTemplateEntry(..)
                          , DataTemplateEntryKey(..))
                          


-- | MigrationClasses are about migrating data not data templates so worry about only the output

data FormVersionZeroEntry = FormVersionZeroEntry { vzKey :: DataTemplateEntryKey
                                                 , vzValue :: FormVersionZero  }

data FormVersionZero = FormVersionZero { signature_1 :: Text
                                       , nameOfWaterHaulingCompany_1 ::Text
                                       , flowbackWater_1 :: Text
                                       , pitWater_1 :: Text
                                       , truckNumber_1 :: Text
                                       , date_1 ::Text
                                       , leaseName_1 :: Text
                                       , waterHaulingPermit_1 :: Text
                                       , bblsProducedWater_1 :: Text
                                       , driverSignature_1 :: Text
                                       , timeIn_1 :: Text
                                       , freshWater_1 :: Text
                                       , nameOfLeaseOperator_1 :: Text  }                                
