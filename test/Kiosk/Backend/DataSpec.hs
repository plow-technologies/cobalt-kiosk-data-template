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
import           Kiosk.Backend.Form              
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

  describe (nameBase 'fromJSONToDataTemplate ++ " IPAD Serialization Test") $
   it "check to make sure IPAD serialization matches ours" $ do
     let
       (Right result) = testJSONIpadEncoding
       recodedJSON    = encode result
     (decodeToValue recodedJSON) `shouldBe` (decodeToValue testJSON)

testJSONIpadEncoding = fromJSONToDataTemplate testJSON

decodeToValue v = decode v :: Maybe Value

-- testGenerateDataTemplate :: IO ByteString
testGenerateDataTemplate = do
  forms <- generate.generateForm $ Static
  let
    forms = [defaultForm]
    dataTemplates = fromFormToDataTemplate <$> forms
  return $ encode $ dataTemplates
  
