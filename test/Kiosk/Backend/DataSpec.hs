{-# LANGUAGE TemplateHaskell #-}
module Kiosk.Backend.DataSpec (main, spec, testGenerateDataTemplate) where

import           Control.Applicative             ((<$>))
import           Data.Aeson                      (Value (..), decode,
                                                  eitherDecode, encode, toJSON)
import           Data.ByteString.Lazy.Internal   (ByteString)

import           Control.Arrow                   ((***))
import           Generators                      (GeneratorType (..),
                                                  generateDataTemplateEntry,
                                                  generateForm)
import           Kiosk.Backend.Data              (DataTemplateEntry (..))
import           Kiosk.Backend.Data.DataTemplate (Appender (..),
                                                  DataTemplate (..),
                                                  decodeObjectAsTemplateItems,
                                                  fromFormToDataTemplate,
                                                  fromJSONToDataTemplate,
                                                  makeUniqueLabels,
                                                  unmakeUniqueLabels)
import           Kiosk.Backend.Form
import           Language.Haskell.TH
import           Mocks.Primitive.Generators      (generateTexts)
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
      isEmpty `shouldBe` False

  describe (nameBase 'fromFormToDataTemplate) $
    it "should transform a Actual Onping Form to a DataTemplate" $ do
      let
        forms = [defaultForm]
        dataTemplates = fromFormToDataTemplate <$> forms
        isEmpty = null dataTemplates
      isEmpty `shouldBe` False

  describe (nameBase ''DataTemplate ++ " Static Aeson Serialization Test") $
   it "should serialize data and be consistent" $ do
     forms <- generate.generateForm $ Static
     let
       restrictedForms = take 1 forms
       dataTemplates = fromFormToDataTemplate <$> restrictedForms
     (tst) <- runAesonSerializationTest dataTemplates "aeson-datatemplate.json"
     putStrLn "\nencode :" >> print (toJSON dataTemplates)
     let x = ((eitherDecode . encode $ dataTemplates)::Either String [DataTemplate])
     case x of
      Left e-> print e
      Right r->print (toJSON r)
     putStrLn "\nTest JSON :" >> print tst
     putStrLn "\nDataTemplates:" >> print dataTemplates
     tst `shouldBe` (Right dataTemplates)

  describe (nameBase ''DataTemplate ++ " Dynamic Aeson Serialization Test") $
   it "should serialize data and be consistent for multiple inputs" $ do
     (tst,expected) <- encodeDecodeDataTemplate
     let (tst_companies,expected_companies) = ((fmap.fmap $ company) *** (fmap.fmap $ company)) (tst,expected)
         (tst_address,expected_address) = ((fmap.fmap $ address) *** (fmap.fmap $ address)) (tst,expected)
         (tst_items,expected_items) = ((fmap.fmap $ templateItems) *** (fmap.fmap $ templateItems )) (tst,expected)
     tst_companies `shouldBe` expected_companies
     tst_address `shouldBe` expected_address
     tst_items `shouldBe` expected_items
     tst `shouldBe` expected

  describe (nameBase ''DataTemplateEntry ++ " Aeson Serialization Test") $
   it "should serialize the entry type and be consistent" $ do
     entries <- generate.generateDataTemplateEntry $ Static
     let
       restrictedEntries = take 1 entries
     (tst) <- runAesonSerializationTest restrictedEntries "aeson-datatemplateentry.json"
     tst `shouldBe` (Right restrictedEntries)

  describe (nameBase 'makeUniqueLabels ++ " " ++ nameBase 'unmakeUniqueLabels) $ do
   it "should return the same label it started with"$ do
     txts <- generate $ generateTexts Dynamic
     let coded = makeUniqueLabels AppendUnderScoredNumber txts
         uncoded = unmakeUniqueLabels AppendUnderScoredNumber <$> coded
     txts `shouldBe` uncoded


  describe (nameBase 'fromJSONToDataTemplate ++ " IPAD Serialization Test") $
   it "check to make sure IPAD serialization matches ours" $ do
     let
       (Right result) = testJSONIpadEncoding
       recodedJSON    = encode result
     decodeToValue recodedJSON `shouldBe` decodeToValue testJSON


encodeDecodeDataTemplate :: IO (Either String [DataTemplate],Either String [DataTemplate])
encodeDecodeDataTemplate = do
       forms <- generate.generateForm $ Dynamic
       let
         makeDataTemplates :: [Form] -> [DataTemplate]
         makeDataTemplates restrictedForms 
           |null restrictedForms = [] 
           |otherwise = fromFormToDataTemplate <$> 
                        take 1 restrictedForms
         tst = eitherDecode . encode . makeDataTemplates $ forms
       return (tst,Right . makeDataTemplates $ forms)


testJSONIpadEncoding :: Either String DataTemplate
testJSONIpadEncoding = fromJSONToDataTemplate testJSON

decodeToValue :: ByteString -> Maybe Value
decodeToValue v = decode v :: Maybe Value

testGenerateDataTemplate :: IO ByteString
testGenerateDataTemplate = do
  _forms <- generate.generateForm $ Dynamic
  let
    forms = [defaultForm]
    dataTemplates = fromFormToDataTemplate <$> forms
  return . encode $ dataTemplates

