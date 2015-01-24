{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Data.MigrationsSpec () where

import Test.Hspec
import Language.Haskell.TH

import Kiosk.Backend.Data.Migrations (FormVersionZeroEntry(..))
main :: IO ()
main = hspec spec

spec :: Spec
spec = describe (nameBase 'FormVersionZeroEntry) $
          it "checks that aeson deserializes to data" $ do          
          True `shouldBe` False


