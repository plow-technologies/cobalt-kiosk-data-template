{-# LANGUAGE TemplateHaskell #-}
module Kiosk.Backend.DataSpec (main, spec, testGenerateDataTemplate) where

import           Control.Applicative             ((<$>))
import           Data.Aeson                      (Value (..), decode, encode,
                                                  toJSON)
import           Data.ByteString.Lazy.Internal   (ByteString)

import           Generators                      (GeneratorType (..),
                                                  generateDataTemplateEntry
                                                 , generateForm
                                                )
import           Mocks.Primitive.Generators (generateTexts)
import           Kiosk.Backend.Data              (DataTemplateEntry (..))
import           Kiosk.Backend.Data.DataTemplate (DataTemplate (..),
                                                  decodeObjectAsTemplateItems,
                                                  fromFormToDataTemplate,
                                                  fromJSONToDataTemplate
                                                 ,unmakeUniqueLabels
                                                 ,makeUniqueLabels
                                                 ,Appender(..))
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

  describe (nameBase ''DataTemplate ++ " Aeson Serialization Test") $
   it "should serialize data and be consistent" $ do
     forms <- generate.generateForm $ Static
     let
       restrictedForms = take 8 forms
       dataTemplates = fromFormToDataTemplate <$> restrictedForms
     (Right tst) <- runAesonSerializationTest dataTemplates "aeson-datatemplate.json"
     tst `shouldBe` dataTemplates

  describe (nameBase ''DataTemplateEntry ++ " Aeson Serialization Test") $
   it "should serialize the entry type and be consistent" $ do
     entries <- generate.generateDataTemplateEntry $ Static
     let
       restrictedEntries = take 8 entries
     (Right tst) <- runAesonSerializationTest restrictedEntries "aeson-datatemplateentry.json"
     tst `shouldBe` restrictedEntries
  describe (nameBase 'makeUniqueLabels ++ " " ++ nameBase 'unmakeUniqueLabels) $ do 
   it "should return the same label it started with"$ do 
     txts <- generate $ generateTexts Dynamic
     let coded = makeUniqueLabels AppendUnderScoredNumber txts
         uncoded = unmakeUniqueLabels AppendUnderScoredNumber coded
     txts `shouldBe` uncoded


  describe (nameBase 'fromJSONToDataTemplate ++ " IPAD Serialization Test") $
   it "check to make sure IPAD serialization matches ours" $ do
     let
       (Right result) = testJSONIpadEncoding
       recodedJSON    = encode result
     decodeToValue recodedJSON `shouldBe` decodeToValue testJSON

testJSONIpadEncoding :: Either String DataTemplate
testJSONIpadEncoding = fromJSONToDataTemplate testJSON

decodeToValue :: ByteString -> Maybe Value
decodeToValue v = decode v :: Maybe Value

testGenerateDataTemplate :: IO ByteString
testGenerateDataTemplate = do
  _forms <- generate.generateForm $ Static
  let
    forms = [defaultForm]
    dataTemplates = fromFormToDataTemplate <$> forms
  return . encode $ dataTemplates

