{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Kiosk.Backend.Data.Migrations.StandardizationSpec (spec,main) where

import           Data.Aeson                    (eitherDecode, encode)
import           Language.Haskell.TH
import           Test.Hspec
-- import           Test.HUnit

import Kiosk.Backend.Data.DataTemplate (TemplateItem(..))
import Kiosk.Backend.Form
import Data.UUID
import           Kiosk.Backend.Data            (DataTemplateEntry (..)
                                               ,DataTemplate(..)

                                               ,TicketId(..)
                                               ,DataTemplateEntryKey(..))
import           Kiosk.Backend.Data.Migrations ( CobaltBaseFormEntry (..),
                                                FormVersionZeroEntry (..) )
import Data.Text (Text,pack,unpack)                                               
import Control.Applicative 
import Kiosk.Backend.Data.Migrations.Standardization
import           Kiosk.Backend.Data.MigrationClass 
import Text.Parser.Char
import Data.Attoparsec.Text (Parser
                            ,parseOnly)
import Text.Parser.Combinators
import Text.Parser.Token (textSymbol)
import Control.Applicative
import Text.Parser.Permutation
import Data.Either.Validation
import           TestImport
import Regex.Genex
import Test.QuickCheck
import Control.Arrow ((***))
main :: IO ()
main = hspec spec



-- * Code 
spec :: Spec
spec = describe (nameBase 'regexStandardizer) $ do
         it "should return the Standard form for any of the strings generated by the target" $ 
          checkStandardizer


regexStringGenerator :: String -> Gen String
regexStringGenerator regexString = elements . genexPure $ [regexString]

regexTextGenerator :: Text -> Gen Text
regexTextGenerator regexText = fmap pack . elements . genexPure $ [unpack regexText]

testParser :: Parser Text
testParser = textSymbol "test"

bigStarText :: Text
bigStarText = "Big Star Company"

bigStarTextUnstandard :: Text 
bigStarTextUnstandard = "[Bb][iI][gG] [Ss][tT][aA][rR] Company"                      

checkStandardizer = forAll (regexTextGenerator bigStarTextUnstandard) (\t -> standardize testStandardizer t == bigStarText)
  where
    testStandardizer = regexStandardizer bigStarTextUnstandard bigStarText
