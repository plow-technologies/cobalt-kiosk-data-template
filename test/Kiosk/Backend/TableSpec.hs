{-# LANGUAGE TemplateHaskell #-}

module Kiosk.Backend.TableSpec (main, spec) where

import Kiosk.Backend.Data
import Kiosk.Backend.Data.DataTemplate (fromFormToDataTemplate)
import           Language.Haskell.TH
import           Test.Hspec
import Data.Table 
import Control.Lens  
import Generators 
import           Test.QuickCheck       
                 
                 
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe (nameBase 'fromFormToDataTemplate) $
    it "should transform a Form to a DataTemplate" $ do
     False `shouldBe` False





testTable :: IO TemplateTable
testTable = do templateEntries <- generate $ generateDataTemplateEntry Static
               return $ (take 10 templateEntries) ^. table
