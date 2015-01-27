{-# LANGUAGE OverloadedStrings #-}
module TestImport  ( testCompany
                   , testData
                   , testJSON
                   , testDataTemplateEntryJSON
                   , testFormVersionZeroDataTemplateEntry
                   ) where

import           Data.ByteString.Lazy.Internal (ByteString)

testCompany :: ByteString
testCompany = "{\"company\":\"testCompany\"}"


testData :: ByteString
testData = "{\"Name_of_Lease_Operator_1\":\"Scott\",\"Field_Name_1\":\"Ling's Oilfield\",\"Flowback_Water_1\":10,\"Lease_Name_1\":\"Lease\",\"Water_Hauling_Permit_1\":5678,\"BBLS_Produced_Water_1\":100,\"Other_1\":\"Notes notes notes\",\"Fresh_Water_1\":10,\"Name_of_Water_Hauling_Company_1\":\"Plowtech Hauling\",\"Date_1\":\"12/12/2014\",\"Drivers_Signature_1\":\"James Haver\",\"Truck_1\":\"1234\",\"Time_in_1\":\"Yesterday\",\"Pit_Water_1\":20}"

testJSON :: ByteString
testJSON = "{\"data\":{\"Name_of_Lease_Operator_1\":\"Scott\",\"Field_Name_1\":\"Ling's Oilfield\",\"Flowback_Water_1\":10,\"Lease_Name_1\":\"Lease\",\"Water_Hauling_Permit_1\":5678,\"BBLS_Produced_Water_1\":100,\"Other_1\":\"Notes notes notes\",\"Fresh_Water_1\":10,\"Name_of_Water_Hauling_Company_1\":\"Plowtech Hauling\",\"Date_1\":\"12/12/2014\",\"Drivers_Signature_1\":\"James Haver\",\"Truck_1\":\"1234\",\"Time_in_1\":\"Yesterday\",\"Pit_Water_1\":20}}"

testDataTemplateEntryJSON :: ByteString
testDataTemplateEntryJSON = "{\"value\":{\"data\":{\"Name_of_Lease_Operator_1\": \"Scott\",\"Field_Name_1\": \"Ling's_Oilfield\", \"Flowback_Water_1\": 10},\"address\":\"testAddress\",\"company\":\"testCompany\"},\"key\":{\"uuid\":\"00000000-0000-0000-0000-000000000000\",\"date\":\"1418937506\",\"formid\":\"1\",\"ticketid\":\"11-12\"}}"


testFormVersionZeroDataTemplateEntry :: ByteString
testFormVersionZeroDataTemplateEntry  =  "{\"value\":{\"data\":{\"BBLS:_Produced_Water_1\": \"25\",\"Date_1\": \"1-26-15\", \"Driver_Signature_1\": \"\"},\"Flowback_Water_1\":\"\",\"Fresh_Water_1\":\"\", \"Lease_Name_1\": \"H-5 oil batt\",\"Name_of_Lease_Operator_1\": \"Citation\",\"Name_of_Water_Hauling_Company_1\": \"Brady inc\",\"Pit_Water_1\": \"\",\"Time_in_1\": \"\",\"Truck_#_1\": \"31\",\"Water_Hauling_Permit_#_1\": \"\", \"signature_1\": \"test_signature\"},\"key\":{\"uuid\":\"a2e3609e-154d-4e60-80e0-c77189098617\",\"date\":\"1422308331184\",\"formid\":\"1\",\"ticketid\":\"1-1245\"}}"


