module Kiosk.Backend.TableSpec (main, spec) where

import           Kiosk.Backend.Data


import           Control.Lens


import           Data.Table  
import           Generators (generateDataTemplateEntry
                            ,GeneratorType(..))
import           Test.Hspec
import           Test.Serial

import           Test.QuickCheck


main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
     describe "Fakey Fae" $ 
      it "fka fka" $ do
       True `shouldBe` True

spec' :: Spec
spec' = do
  describe ("Serialization test TemplateTable") $
    it "should transform a Form to a DataTemplate" $ do
     tables <- testTable
     rslt <- runCerealSerializationTest tables "template-table.serial"
     rslt `shouldBe` (Right tables)



testTableEntries :: IO [DataTemplateEntry]
testTableEntries = generate $ generateDataTemplateEntry Static

testTable :: IO TemplateTable
testTable = do templateEntries <- testTableEntries
               return $   getTemplateTable  #  ( (take 10 templateEntries) ^. table)
