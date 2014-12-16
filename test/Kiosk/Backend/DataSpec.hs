module Kiosk.Backend.DataSpec (main, spec) where

import           Generators (generateForm
                            ,GeneratorType)
import Kiosk.Backend.Data (fromFormToDataTemplate)
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "fromFormToDataTemplate" $ do
    it "should transform a Form to a DataTemplate" $ do      
      True `shouldBe` False
