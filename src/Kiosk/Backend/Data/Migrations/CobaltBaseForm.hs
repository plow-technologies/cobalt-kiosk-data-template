{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}

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

module Kiosk.Backend.Data.Migrations.CobaltBaseForm (CobaltBaseFormEntry (..)) where        

import Kiosk.Backend.Data.MigrationClass
import Data.Text (Text)
import Data.Aeson (ToJSON,FromJSON)
import GHC.Generics
import Data.Either.Validation
import Data.Monoid ((<>))
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

data WaterType = PitWater | FlowBackWater | FreshWater | ProducedWater 
  deriving (Show,Enum,Eq,Generic)

instance ToJSON WaterType where 
instance FromJSON WaterType where 

data CobaltBaseFormEntry = CobaltBaseFormEntry { cobaltBaseKey   :: DataTemplateEntryKey
                                               , cobaltBaseValue :: CobaltBaseForm  }
                         deriving (Show,Eq,Generic) 
instance ToJSON CobaltBaseFormEntry where
instance FromJSON CobaltBaseFormEntry where


data CobaltBaseForm = CobaltBaseForm { _nameOfWaterHaulingCompany :: T.Text
                                     , _amount                    :: Double
                                     , _customerTicketNumber      :: T.Text                               
                                     , _date                      :: T.Text
                                     , _timeIn                    :: T.Text
                                     , _typeOfWaterHauled         :: WaterType
                                     , _truckNumber               :: T.Text
                                     , _waterHaulingPermit        :: T.Text
                                     , _nameOfLeaseOperator       :: T.Text
                                     , _leaseName                 :: T.Text
                                     , _signature                 :: T.Text  }
                    deriving (Show,Eq,Generic)

instance ToJSON CobaltBaseForm where
instance FromJSON CobaltBaseForm where

instance ToDataTemplate CobaltBaseFormEntry where 
  toDataTemplate = fromCobaltBaseForm

-- | MigrationClasses are about migrating data not data templates so worry about only the output

fromCobaltBaseForm :: CobaltBaseFormEntry -> DataTemplateEntry
fromCobaltBaseForm f1e = DataTemplateEntry dtKey dtValue
                 where dtKey = cobaltBaseKey f1e
                       dtValue = convertCobaltBaseFormToDataTemplate (cobaltBaseValue f1e)



convertCobaltBaseFormToDataTemplate ::  CobaltBaseForm -> DataTemplate
convertCobaltBaseFormToDataTemplate fv1 = dataTemplateBuilder fv1

dataTemplateBuilder :: CobaltBaseForm -> DataTemplate
dataTemplateBuilder fv1 = DataTemplate $ convertFormToTemplateItems fv1

printWaterType :: WaterType -> String
printWaterType wt = case wt of
           PitWater      -> "Pit Water"
           FlowBackWater -> "Flow Back Water"
           FreshWater    -> "Fresh Water"
           ProducedWater -> "Produced Water"


convertFormToTemplateItems :: CobaltBaseForm -> [TemplateItem]
convertFormToTemplateItems fv1 = [nItem, a , customerTicketNumber , d ,tin , twh, tn, nlo, ln, s ]
                        where nItem = makeTemplateItemText "Name_of_Water_Hauling_Company" (_nameOfWaterHaulingCompany fv1)
                              a     = makeTemplateItemDouble "Amount" (_amount fv1)
                              customerTicketNumber = makeTemplateItemText "Customer_Ticket_Number" (_customerTicketNumber fv1)
                              d     = makeTemplateItemText "Date" (_date fv1)
                              tin   = makeTemplateItemText "Time_in" (_timeIn fv1)
                              twh   = makeTemplateItemText "Type_of_Water_Hauled" (T.pack . printWaterType . _typeOfWaterHauled $ fv1)
                              tn    = makeTemplateItemText "Water_Hauling_Permit#" (_waterHaulingPermit fv1)
                              nlo   = makeTemplateItemText "Name_of_Lease_Operator" (_nameOfLeaseOperator fv1)
                              ln    = makeTemplateItemText "Lease_Name" (_leaseName fv1)
                              s     = makeTemplateItemText "Driver_Signature" (_signature fv1)

fromWaterTypeFoundTextToDouble
  :: WaterTypeFound Text -> Validation String (WaterTypeFound Double)
fromWaterTypeFoundTextToDouble wtf@(WaterTypeFound _ txt) = eitherToValidation eitherDouble 
  where 
   eitherDouble :: Either String (WaterTypeFound Double)
   eitherDouble = putBackInWaterTypeFound.fst <$> T.double txt
   putBackInWaterTypeFound :: Double -> WaterTypeFound Double
   putBackInWaterTypeFound d =  wtf {getAmount = d}
                                    
  

formVersionZeroEntryToCobaltBaseEntry
  :: FormVersionZeroEntry
     -> Validation
          (MigrationError Text (Maybe FormVersionZeroEntry)) CobaltBaseFormEntry
formVersionZeroEntryToCobaltBaseEntry v0e@(FormVersionZeroEntry v0EntryKey v0Form) = over _Failure makeErrorEntry (makeVOne <$> formVersionZeroToCobaltBaseForm v0Form)
  where 
   makeErrorEntry (MigrationError e _) = MigrationError e (Just v0e)
   makeVOne  = CobaltBaseFormEntry v0EntryKey{ _getFormId = 1 }   

formVersionZeroToCobaltBaseForm :: FormVersionZero -> Validation (MigrationError Text (Maybe FormVersionZero)) CobaltBaseForm
formVersionZeroToCobaltBaseForm  v0@(FormVersionZero { _signature_1                 
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
                         makeMigrationError :: String -> MigrationError Text (Maybe FormVersionZero)
                         makeMigrationError str = MigrationError (T.pack str) (Just v0)
                         decodeWithCorrectWaterType (WaterTypeFound wt amt)  = 
                           return $ (CobaltBaseForm { _nameOfWaterHaulingCompany = _nameOfWaterHaulingCompany_1
                                                    , _amount = amt
                                                    , _customerTicketNumber = "" -- New Field
                                                    , _date = _date_1
                                                    , _timeIn = _timeIn_1
                                                    , _typeOfWaterHauled = wt
                                                    , _truckNumber = _truckNumber_1
                                                    , _waterHaulingPermit = _waterHaulingPermit_1
                                                    , _nameOfLeaseOperator = _nameOfLeaseOperator_1
                                                    , _leaseName = _leaseName_1
                                                    , _signature = _signature_1})                                                            



validateWaterTypeOnlyOneFull :: FormVersionZero -> Validation (MigrationError Text (Maybe FormVersionZero)) (WaterTypeFound Text)
validateWaterTypeOnlyOneFull v0@(FormVersionZero { _flowbackWater_1             
                                                              , _pitWater_1                  
                                                              , _bblsProducedWater_1         
                                                              , _freshWater_1                
                                                              }) = over _Failure  (\t -> MigrationError t (Just v0)) waterTypeAndForm
  where 
    waterTypeAndForm ::Validation Text   (WaterTypeFound Text)
    waterTypeAndForm = waterTypeSearchToValidation selectOneWaterType
    allWaterText = [_flowbackWater_1
                   , _pitWater_1
                   , _bblsProducedWater_1
                   , _freshWater_1]                   
    selectOneWaterType = foldl' findOneValidWaterType (TestThisWaterType PitWater) allWaterText


findOneValidWaterType :: WaterTypeSearch -> Text -> WaterTypeSearch                         
findOneValidWaterType (TestThisWaterType ProducedWater) possibleWaterText
   |T.null possibleWaterText = WaterTypeError ("did not decode " <> possibleWaterText <> " to any valid water type")
   | otherwise = FoundThisWaterType ProducedWater possibleWaterText
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

instance FormMigration FormVersionZeroEntry CobaltBaseFormEntry where
  transformRecord = formVersionZeroEntryToCobaltBaseEntry



