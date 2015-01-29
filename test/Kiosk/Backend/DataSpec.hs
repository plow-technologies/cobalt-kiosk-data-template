{-# LANGUAGE TemplateHaskell #-}
module Kiosk.Backend.DataSpec (main, spec, testGenerateDataTemplate, encodeDecodeHashTests) where

import           Control.Applicative             ((<$>))
import           Data.Aeson                      (Value (..), decode,
                                                  eitherDecode, encode, toJSON)
import           Data.ByteString.Lazy.Internal   (ByteString)
import           Data.Text                       (Text)

import           Control.Arrow                   ((***))
import qualified Data.HashMap.Strict             as HM
import           Data.List                       (sort)
import           Generators                      (GeneratorType (..), checkStaticGeneratorConsistency,
                                                  generateDataTemplateEntry,
                                                  generateForm)
import           Kiosk.Backend.Data              (DataTemplateEntry (..))
import           Kiosk.Backend.Data.DataTemplate (DataTemplate (..),
                                                  fromFormToDataTemplate,
                                                  fromJSONToDataTemplate)
import           Kiosk.Backend.Form
import           Language.Haskell.TH
import           Test.Hspec
import           Test.QuickCheck
import           TestImport                      (testJSON)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do 
  describe (nameBase 'fromFormToDataTemplate) $
    it "should transform a Actual Onping Form to a DataTemplate" $ do
      let
        forms = [defaultForm]
        dataTemplates = fromFormToDataTemplate <$> forms
        isEmpty = null dataTemplates
      isEmpty `shouldBe` False
  describe (nameBase ''DataTemplate ++ " Dynamic Aeson Serialization Test") $
   it "should serialize data and be consistent for multiple inputs" $ do
     (tst,expected) <- encodeDecodeDataTemplate
     let (tst_items,expected_items) = ((fmap.fmap $ templateItems) *** (fmap.fmap $ templateItems )) (tst,expected)
     (sort.concat <$> tst_items) `shouldBe` (sort.concat <$> expected_items)
     tst `shouldBe` expected
  describe (nameBase 'fromJSONToDataTemplate ++ " IPAD Serialization Test") $
   it "check to make sure IPAD serialization matches ours" $ do
     let
       (Right result) = testJSONIpadEncoding
       recodedJSON    = encode result
     decodeToValue recodedJSON `shouldBe` decodeToValue testJSON


encodeDecodeDataTemplate :: IO (Either String [DataTemplate],Either String [DataTemplate])
encodeDecodeDataTemplate = do
       forms <- generate.generateForm $ Static
       let
         makeDataTemplates :: [Form] -> [DataTemplate]
         makeDataTemplates restrictedForms = fromFormToDataTemplate <$>
                                             take 1 restrictedForms
         tst = eitherDecode . encode . makeDataTemplates $ forms
       return (tst,Right . makeDataTemplates $ forms)


encodeDecodeDataTemplateEntry :: IO (Either String [DataTemplateEntry],Either String [DataTemplateEntry])
encodeDecodeDataTemplateEntry = do
       entries <- fmap (take 1) . generate $ generateDataTemplateEntry Static
       let tst = eitherDecode . encode $ entries
       return (tst,Right entries )

encodeDecodeHashTests :: IO [(Text, Value)]
encodeDecodeHashTests = do
  entries <- fmap (take 1) . generate $ generateDataTemplateEntry Static
  let (Object tst) = toJSON.head $ entries
  return $ HM.foldlWithKey' listSequenceCheck [] tst
    where
      listSequenceCheck lst k v  = (k,v):lst

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



-- | Tests to fix later concerning problems with hashing


spec' :: Spec
spec' = do
  describe (nameBase 'checkStaticGeneratorConsistency) $
    it "should check that the generative tests hold equivalence for static cases" $
      property $ checkStaticGeneratorConsistency  

  describe (nameBase ''DataTemplateEntry ++ " Dynamic Aeson Test") $
   it "should show that serialization works for lots of tests" $ do
     (tst,expected) <- encodeDecodeDataTemplateEntry
     tst `shouldBe` expected   
