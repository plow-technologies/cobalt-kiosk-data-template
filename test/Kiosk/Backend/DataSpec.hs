module Kiosk.Backend.DataSpec (main, spec) where

import           Generators (generateForm
                            ,GeneratorType(..))
import Kiosk.Backend.Data (fromFormToDataTemplate)
import           Test.Hspec
import           Test.QuickCheck
import Control.Applicative ((<$>))
import Data.Aeson (encode)

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
--      print restrictedForms
      print . encode $  dataTemplates        
      isEmpty `shouldBe` False
