{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Kiosk.Backend.Data.Migrations.NormalizationSpec (spec,main) where

import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "test" $ do
        it "should work" $ do
         True `shouldBe` True


