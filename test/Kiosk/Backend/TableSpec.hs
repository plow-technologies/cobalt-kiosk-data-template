{-# LANGUAGE TemplateHaskell #-}

module Kiosk.Backend.TableSpec (main, spec) where

import Kiosk.Backend.Data


import           Test.Hspec
import Data.Table 
import Control.Lens  

import Test.Serial              
import Generators 

import           Test.QuickCheck       
                 
                 
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe ("Serialization test TemplateTable") $
    it "should transform a Form to a DataTemplate" $ do
     tables <- testTable
     runCerealSerializationTest tables "template-table.serial"
     False `shouldBe` False





testTable :: IO TemplateTable
testTable = do templateEntries <- generate $ generateDataTemplateEntry Static
               return $   getTemplateTable  #  ( (take 10 templateEntries) ^. table)


