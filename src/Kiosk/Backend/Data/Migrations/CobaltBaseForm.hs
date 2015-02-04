{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      :  Kiosk.Backend.Data.Migrations.CobaltBaseForm
Description :  Base Form for all company forms to follow
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

Basically, I don't want to do the work of changing every single form to match the 
company so we are going to have one form that will be translated into the 
various company forms

-}

module Kiosk.Backend.Data.Migrations.CobaltBaseForm (FormVersionOneEntry (..)) where        


import Kiosk.Backend.Data.MigrationClass

import Data.Text (Text)

import Data.Either.Validation

import Data.List (foldl')

import Control.Lens (over)

import Control.Applicative ((<$>))

import qualified Data.Text                         as T

import qualified Data.Text.Read as T

import           Kiosk.Backend.Data.DataTemplate  ( TemplateItem (..)
                                                  , DataTemplate (..)
                                                    )
                                                  
import           Kiosk.Backend.Data                (DataTemplateEntry (..),
                                                    DataTemplateEntryKey (..)

                                                    )

import Kiosk.Backend.Data.Migrations.FormVersionZero ( FormVersionZeroEntry(..)
                                                     , FormVersionZero(..))

data WaterType = PitWater | FlowBackWater | FreshWater | ProducedWater deriving (Show,Enum)

data FormVersionOneEntry = FormVersionOneEntry { versionOneKey   :: DataTemplateEntryKey
                                               , versionOneValue :: FormVersionOne  }

data FormVersionOne = FormVersionOne { nameOfWaterHaulingCompany :: T.Text
                                     , amount                    :: Double
                                     , date                      :: T.Text
                                     , timeIn                    :: T.Text
                                     , typeOfWaterHauled         :: WaterType
                                     , _truckNumber               :: T.Text
                                     , waterHaulingPermit        :: T.Text
                                     , nameOfLeaseOperator       :: T.Text
                                     , leaseName                 :: T.Text
                                     , signature                 :: T.Text  }


instance ToDataTemplate FormVersionOneEntry where 
  toDataTemplate = fromFormVersionOne


-- | MigrationClasses are about migrating data not data templates so worry about only the output



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

convertFormToTemplateItems :: FormVersionOne -> [TemplateItem]
convertFormToTemplateItems fv1 = [nItem, a , d ,tin , twh, tn, nlo, ln, s ]
                        where nItem = makeTemplateItemText "Name_of_Water_Hauling_Company" (nameOfWaterHaulingCompany fv1)
                              a     = makeTemplateItemDouble "Amount" (amount fv1)
                              d     = makeTemplateItemText "Date" (date fv1)
                              tin   = makeTemplateItemText "Time_in" (timeIn fv1)
                              twh   = makeTemplateItemText "Type_of_Water_Hauled" (T.pack . printWaterType . typeOfWaterHauled $ fv1)
                              tn    = makeTemplateItemText "Water_Hauling_Permit#" (waterHaulingPermit fv1)
                              nlo   = makeTemplateItemText "Name_of_Lease_Operator" (nameOfLeaseOperator fv1)
                              ln    = makeTemplateItemText "Lease_Name" (leaseName fv1)
                              s     = makeTemplateItemText "Driver_Signature" (signature fv1)


fromWaterTypeFoundTextToDouble
  :: WaterTypeFound Text -> Validation String (WaterTypeFound Double)
fromWaterTypeFoundTextToDouble wtf@(WaterTypeFound _ txt) = eitherToValidation eitherDouble 
  where 
   eitherDouble :: Either String (WaterTypeFound Double)
   eitherDouble = putBackInWaterTypeFound.fst <$> T.double txt
   putBackInWaterTypeFound :: Double -> WaterTypeFound Double
   putBackInWaterTypeFound d =  wtf {getAmount = d}
                                    
  
formVersionZeroEntryToFormOneEntry
  :: FormVersionZeroEntry
     -> Validation
          (MigrationError Text FormVersionZeroEntry) FormVersionOneEntry
formVersionZeroEntryToFormOneEntry v0e@(FormVersionZeroEntry v0EntryKey v0Form) = over _Failure makeErrorEntry (makeVOne <$> formVersionZeroToFormVersionOne v0Form)
  where 
   makeErrorEntry (MigrationError e _) = MigrationError e v0e
   makeVOne  = FormVersionOneEntry v0EntryKey{ _getFormId = 1 }   


formVersionZeroToFormVersionOne :: FormVersionZero -> Validation (MigrationError Text FormVersionZero) FormVersionOne
formVersionZeroToFormVersionOne  v0@(FormVersionZero { _signature_1                 
                                                     , _nameOfWaterHaulingCompany_1 
                                                     , _flowbackWater_1             
                                                     , _pitWater_1                  
                                                     , _truckNumber_1               
                                                     , _date_1                      
                                                     , _leaseName_1                 
                                                     , _waterHaulingPermit_1        
                                                     , _bblsProducedWater_1         
                                                     , _driverSignature_1           
                                                     , _timeIn_1                    
                                                     , _freshWater_1                
                                                     , _nameOfLeaseOperator_1        
                                                      }) =  
    eitherToValidation $ validationToEither (validateWaterTypeOnlyOneFull  v0) >>=
                (\wt -> validationToEither (over _Failure makeMigrationError (fromWaterTypeFoundTextToDouble wt))) >>= 
                decodeWithCorrectWaterType 
                       where
                         makeMigrationError :: String -> MigrationError Text FormVersionZero
                         makeMigrationError str = MigrationError (T.pack str) v0
                         decodeWithCorrectWaterType (WaterTypeFound wt amt)  = 
                           return $ (FormVersionOne { nameOfWaterHaulingCompany = _nameOfWaterHaulingCompany_1
                                                    , amount = amt
                                                    , date = _date_1
                                                    , timeIn = _timeIn_1
                                                    , typeOfWaterHauled = wt
                                                    , _truckNumber = _truckNumber_1
                                                    , waterHaulingPermit = _waterHaulingPermit_1
                                                    , nameOfLeaseOperator = _nameOfLeaseOperator_1
                                                    , leaseName = _leaseName_1
                                                    , signature = _signature_1})                                                            



validateWaterTypeOnlyOneFull :: FormVersionZero -> Validation (MigrationError Text FormVersionZero) (WaterTypeFound Text)
validateWaterTypeOnlyOneFull v0@(FormVersionZero { _flowbackWater_1             
                                                              , _pitWater_1                  
                                                              , _bblsProducedWater_1         
                                                              , _freshWater_1                
                                                              }) = over _Failure  (\t -> MigrationError t v0) waterTypeAndForm
  where 
    waterTypeAndForm ::Validation Text   (WaterTypeFound Text)
    waterTypeAndForm = waterTypeSearchToValidation selectOneWaterType
    allWaterText = [_flowbackWater_1
                   , _pitWater_1
                   , _bblsProducedWater_1
                   , _freshWater_1]                   
    selectOneWaterType = foldl' findOneValidWaterType (TestThisWaterType FlowBackWater) allWaterText


findOneValidWaterType :: WaterTypeSearch -> Text -> WaterTypeSearch                         
findOneValidWaterType (TestThisWaterType currentWaterType) possibleWaterText
   |T.null possibleWaterText = TestThisWaterType . succ $ currentWaterType
   | otherwise = FoundThisWaterType currentWaterType possibleWaterText
findOneValidWaterType f@(FoundThisWaterType _ _) possibleWaterText
   | T.null possibleWaterText = f
   | otherwise = WaterTypeError "More Than One kind of watertype found"       
findOneValidWaterType wtErr@(WaterTypeError _) _ = wtErr

waterTypeSearchToValidation :: WaterTypeSearch  -> Validation Text (WaterTypeFound Text)
waterTypeSearchToValidation (TestThisWaterType _) = Failure "No Amount found, all water type are null"                                
waterTypeSearchToValidation (WaterTypeError t) = Failure t
waterTypeSearchToValidation (FoundThisWaterType waterType txt) = Success . WaterTypeFound waterType $ txt

data WaterTypeFound amt = WaterTypeFound {_getWaterType :: WaterType
                                         ,getAmount :: amt }

data WaterTypeSearch = TestThisWaterType WaterType | WaterTypeError  Text | FoundThisWaterType WaterType Text

instance FormMigration FormVersionZeroEntry FormVersionOneEntry where
  transformRecord = formVersionZeroEntryToFormOneEntry


