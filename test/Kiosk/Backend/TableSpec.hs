{-# LANGUAGE TemplateHaskell #-}

module Kiosk.Backend.TableSpec (main, spec) where

import Kiosk.Backend.Data


import           Test.Hspec
import Data.Table 
import Control.Lens  
import Data.Foldable
import Test.Serial              
import Generators 
import Data.Aeson

import           Test.QuickCheck       
                 
                 
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe ("Serialization test TemplateTable") $
    it "should transform a Form to a DataTemplate" $ do
     tables <- testTable
     (Right rslt) <- runCerealSerializationTest tables "template-table.serial"
     rslt `shouldBe` tables



testTableEntries :: IO [DataTemplateEntry]
testTableEntries = generate $ generateDataTemplateEntry Static
testTable :: IO TemplateTable
testTable = do templateEntries <- testTableEntries
               return $   getTemplateTable  #  ( (take 10 templateEntries) ^. table)


