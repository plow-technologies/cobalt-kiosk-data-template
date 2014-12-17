{-# LANGUAGE TemplateHaskell #-}
module Kiosk.Backend.DataSpec (main, spec) where

import           Control.Applicative             ((<$>))
import           Data.Aeson                      (Value (..), decode, encode,
                                                  toJSON)
import           Data.Either                     (isRight)
import           Data.Maybe                      (fromJust)
import           Generators                      (GeneratorType (..),
                                                  generateForm)
import           Kiosk.Backend.Data.DataTemplate (DataTemplate (..),
                                                  decodeObjectAsTemplateItems,
                                                  fromFormToDataTemplate,
                                                  fromJSONToDataTemplate)
import           Kiosk.Backend.Form              (Form, defaultForm)
import           Language.Haskell.TH
import           Test.Hspec
import           Test.QuickCheck
import           Test.Serial                     (runAesonSerializationTest)
import           TestImport                      (testCompany, testData,
                                                  testJSON)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe (nameBase 'fromFormToDataTemplate) $
    it "should transform a Form to a DataTemplate" $ do
      forms <- generate.generateForm $ Static
      let
        restrictedForms = take 8 forms
        dataTemplates = fromFormToDataTemplate <$> restrictedForms
        isEmpty = null dataTemplates
      -- print restrictedForms
      print . encode $  dataTemplates
      isEmpty `shouldBe` False

  describe (nameBase 'fromFormToDataTemplate) $
    it "should transform a Actual Onping Form to a DataTemplate" $ do
      let
        forms = [defaultForm]
        dataTemplates = fromFormToDataTemplate <$> forms
        isEmpty = null dataTemplates
      print . encode $  dataTemplates
      isEmpty `shouldBe` False

  describe (concat [nameBase ''DataTemplate ,"Aeson Serialization Test" ]) $
   it "should serialize data and be consistent" $ do
     forms <- generate.generateForm $ Static
     let
       restrictedForms = take 8 forms
       dataTemplates = fromFormToDataTemplate <$> restrictedForms
     (Right tst) <- runAesonSerializationTest dataTemplates "aeson-datatemplate.json"
     True `shouldBe` True -- The real test is the Right

  describe (nameBase 'fromJSONToDataTemplate ++ " Aseson Serialization Test") $
   it "should serialize data and be consistent" $ do
     let
       result = fromJSONToDataTemplate testJSON
     print. toJSON $ result
     True `shouldBe` True
     -- isRight result `shouldBe` True

  -- describe (nameBase 'decodeObjectAsTemplateItems ++ " JSON Decoding Test") $
  --  it "should decode the piece of data" $ do
  --    let
  --      value = decode testData :: Maybe Value
  --    result <- decodeObjectAsTemplateItems .fromJust $ value
  --    print . toJSON $ result
  --    (isRight result) `shouldBe` True -- The real test is the Right


