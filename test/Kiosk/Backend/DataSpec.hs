module Kiosk.Backend.DataSpec (main, spec) where

import           Generators (generateForm
                            ,GeneratorType)
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "someFunction" $ do
    it "should work fine" $ do
      True `shouldBe` False
