{-# LANGUAGE TemplateHaskell #-}
module Kiosk.Backend.DataSpec (main, spec) where

import           Control.Applicative             ((<$>))
import           Data.Aeson                      (encode)
import           Generators                      (GeneratorType (..),
                                                  generateForm)
import           Kiosk.Backend.Data.DataTemplate (DataTemplate (..),
                                                  fromFormToDataTemplate)
import           Kiosk.Backend.Form              (Form, defaultForm)
import           Language.Haskell.TH
import           Test.Hspec
import           Test.QuickCheck
import           Test.Serial                     (runAesonSerializationTest)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe (nameBase 'fromFormToDataTemplate) $ do
    it "should transform a Form to a DataTemplate" $ do
      forms <- generate.generateForm $ Static
      let
        restrictedForms = take 8 forms
        dataTemplates = fromFormToDataTemplate <$> restrictedForms
        isEmpty = null dataTemplates
      -- print restrictedForms
      print . encode $  dataTemplates
      isEmpty `shouldBe` False

  describe (nameBase 'fromFormToDataTemplate) $ do
    it "should transform a Actual Onping Form to a DataTemplate" $ do
      let
        forms = [defaultForm]
        dataTemplates = fromFormToDataTemplate <$> forms
        isEmpty = null dataTemplates
      print . encode $  dataTemplates
      isEmpty `shouldBe` False
  describe (concat [nameBase ''DataTemplate ,"Aeson Serialization Test" ]) $ do
   it "should serialize data and be consistent" $ do
     forms <- generate.generateForm $ Static
     let
       restrictedForms = take 8 forms
       dataTemplates = fromFormToDataTemplate <$> restrictedForms
     (Right tst) <- runAesonSerializationTest dataTemplates "aeson-datatemplate.json"
     True `shouldBe` True -- The real test is the Right



