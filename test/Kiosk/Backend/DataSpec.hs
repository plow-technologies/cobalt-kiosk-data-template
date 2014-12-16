module Kiosk.Backend.DataSpec (main, spec) where

import           Control.Applicative ((<$>))
import           Data.Aeson          (encode)
import           Generators          (GeneratorType (..), generateForm)
import           Kiosk.Backend.Data  (fromFormToDataTemplate)
import           Kiosk.Backend.Form  (defaultForm)
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fromFormToDataTemplate" $ do
    it "should transform a Form to a DataTemplate" $ do
      forms <- generate.generateForm $ Dynamic
      let
        restrictedForms = take 8 forms

        dataTemplates = fromFormToDataTemplate <$> restrictedForms
        isEmpty = null dataTemplates
      -- print restrictedForms
      print . encode $  dataTemplates
      isEmpty `shouldBe` False

  describe "fromFormToDataTemplate" $ do
    it "should transform a Actual Onping Form to a DataTemplate" $ do
      let
        forms = [defaultForm]
        dataTemplates = fromFormToDataTemplate <$> forms
        isEmpty = null dataTemplates
      print . encode $  dataTemplates
      isEmpty `shouldBe` False
