{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Kiosk.Backend.Data.MigrationsSpec (spec,main) where

import           Data.Aeson                    (eitherDecode, encode)
import           Language.Haskell.TH
import           Test.Hspec
-- import           Test.HUnit

import Kiosk.Backend.Data.DataTemplate (TemplateItem(..))
import Kiosk.Backend.Form
import Data.UUID
import           Kiosk.Backend.Data            (DataTemplateEntry (..)
                                               ,DataTemplate(..)
                                               
                                               ,TicketId(..)
                                               ,DataTemplateEntryKey(..))
import           Kiosk.Backend.Data.Migrations ( CobaltBaseFormEntry (..),
                                                FormVersionZeroEntry (..) )
import Data.Text                                                
import Control.Applicative 
import           Kiosk.Backend.Data.MigrationClass 

import Data.Either.Validation
import           TestImport

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe (nameBase 'CobaltBaseFormEntry) $
   it ("Should convert " ++ (nameBase 'CobaltBaseFormEntry) ++ "to" ++ (nameBase 'CobaltBaseFormEntry)) $ do
     let (Right dt) = eitherDecode testFormVersionZeroDataTemplateEntry :: Either String DataTemplateEntry
         (Success fv1) = (transformRecord . fromDataTemplate $ dt) :: Validation (MigrationError Text (Maybe FormVersionZeroEntry)) CobaltBaseFormEntry
         backToDataTemplateEntry :: DataTemplateEntry
         backToDataTemplateEntry = toDataTemplate fv1
     (encode $ backToDataTemplateEntry) `shouldBe` "{\"value\":{\"Name_of_Water_Hauling_Company\":\"Brady inc\",\"Amount\":25,\"Type_of_Water_Hauled\":\"Produced Water\",\"Customer_Ticket_Number\":\"\",\"Lease_Name\":\"H-5 oil batt\",\"Date\":\"1-26-15\",\"Name_of_Lease_Operator\":\"Citation\",\"Time_in\":\"\",\"Driver_Signature\":\"test_signature\",\"Water_Hauling_Permit#\":\"\"},\"key\":{\"uuid\":\"a2e3609e-154d-4e60-80e0-c77189098617\",\"ticketid\":\"1-1245\",\"date\":\"1422308331184\",\"formid\":\"1\"}}"


