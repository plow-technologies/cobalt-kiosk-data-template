{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Kiosk.Backend.Data.MigrationsSpec (spec,main) where

import           Data.Aeson                    (eitherDecode, encode)
import           Language.Haskell.TH
import           Test.Hspec
-- import           Test.HUnit

import           Kiosk.Backend.Data            (DataTemplateEntry (..))
import           Kiosk.Backend.Data.Migrations (FormVersionZeroEntry (..),
                                                toFormVersionZeroEntry)
import           TestImport

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe (nameBase 'DataTemplateEntry) $
   it "Should checks that aeson deserializes to data template" $ do
     let (Right dt) = eitherDecode testFormVersionZeroDataTemplateEntry :: Either String DataTemplateEntry
     putStrLn "\nJSONForm :" >> print testFormVersionZeroDataTemplateEntry
     putStrLn "\nDataTemplateEntry :" >> print dt
     True `shouldBe` True

  describe (nameBase 'FormVersionZeroEntry) $
   it "Should convert DataTemplateEntry to FormVersionZeroEntry" $ do
     let (Right dt) = eitherDecode testFormVersionZeroDataTemplateEntry :: Either String DataTemplateEntry
         fv0 = toFormVersionZeroEntry dt
     putStrLn "\nJSONForm :" >> print fv0
     putStrLn "\nFormVersionZeroEntry :" >> print dt
     True `shouldBe` True


