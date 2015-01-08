{-# LANGUAGE OverloadedStrings #-}
module TestImport  ( testCompany
                   , testData
                   , testJSON
                   , testDataTemplateEntryJSON
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
