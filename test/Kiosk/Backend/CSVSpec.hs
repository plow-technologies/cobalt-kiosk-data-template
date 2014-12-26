{-# LANGUAGE TemplateHaskell #-}

module Kiosk.Backend.CSVSpec (main, spec) where

import           Kiosk.Backend.Data              (DataTemplateEntry,
                                                  dataTemplateEntryValue,
                                                  fromDataTemplateEntryToCsv)
import           Kiosk.Backend.Data.DataTemplate (fromDataTemplateToCSV,
                                                  fromFormToDataTemplate)
import           Kiosk.Backend.Form              (defaultForm)

import           Control.Applicative             ((<$>))
import           Control.Lens                    (view)
import           Data.Aeson                      (eitherDecode)
import qualified Data.ByteString.Lazy            as LBS (ByteString, null)
import           Generators                      (GeneratorType (Static),
                                                  generateDataTemplate)
import           Language.Haskell.TH
import           Test.Hspec
import           Test.QuickCheck                 (generate)
import           TestImport                      (testDataTemplateEntryJSON)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe (nameBase 'fromDataTemplateToCSV ++ " CSV Generator Test") $
    it "should transform a generated DataTemplate to CSV" $ do
       dts <- generate .  generateDataTemplate $ Static
       let rslt = fromDataTemplateToCSV (take 1 dts)
       print rslt
       validateBS rslt `shouldBe` False

  describe (nameBase 'fromDataTemplateToCSV ++ " CSV Actual Test") $
    it "Should tranform actual DataTeplate to a Empty CSV" $ do
       let
        forms = [defaultForm]
        dataTemplates = fromFormToDataTemplate <$> forms
       print dataTemplates
       let rslt = fromDataTemplateToCSV (take 1 dataTemplates)
       print rslt
       validateBS rslt `shouldBe` False

  describe (nameBase 'fromDataTemplateToCSV ++ " CSV Actual Test") $
    it "Should tranform actual DataTemplate to a Actual CSV" $ do
       let
        (Right dtEntry) = eitherDecode testDataTemplateEntryJSON :: Either String DataTemplateEntry
        dt = view dataTemplateEntryValue dtEntry
        rslt = fromDataTemplateToCSV [dt]
       print rslt
       validateBS rslt `shouldBe` False

  describe (nameBase 'fromDataTemplateEntryToCsv ++ " DataTemplateEntry to Csv Test") $
    it "Should tranform actual DataTeplateEntry to a CSV" $ do
       let
        (Right dtEntry) = eitherDecode testDataTemplateEntryJSON :: Either String DataTemplateEntry
        -- dt = view dataTemplateEntryValue dtEntry
        rslt = fromDataTemplateEntryToCsv [dtEntry]
       print dtEntry
       print rslt
       validateBS rslt `shouldBe` False


validateBS :: LBS.ByteString -> Bool
validateBS = LBS.null


