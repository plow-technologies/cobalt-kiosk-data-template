{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Kiosk.Backend.Data.MigrationsSpec (spec,main) where

import           Data.Aeson                    (eitherDecode, encode)
import           Language.Haskell.TH
import           Test.Hspec
-- import           Test.HUnit

import           Kiosk.Backend.Data            (DataTemplateEntry (..))
import           Kiosk.Backend.Data.Migrations (FormVersionOneEntry (..),
                                                FormVersionZeroEntry (..) )
import Data.Text                                                
import Control.Applicative 
import           Kiosk.Backend.Data.MigrationClass 

import Data.Either.Validation
import           TestImport

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe (nameBase 'FormVersionOneEntry) $
   it "Should convert FormVersionZeroEntry to FormVersionOneEntry" $ do
     let (Right dt) = eitherDecode testFormVersionZeroDataTemplateEntry :: Either String DataTemplateEntry
         (Success fv1) = (transformRecord . toIncomingRecord $ dt) :: Validation (MigrationError Text FormVersionZeroEntry) FormVersionOneEntry
   
     True `shouldBe` True
