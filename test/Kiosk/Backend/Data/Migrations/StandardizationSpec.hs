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
import Data.Text                                                
import Control.Applicative ()
import           Kiosk.Backend.Data.MigrationClass 
import Text.Parser.Char
import Data.Attoparsec.Text (Parser
                            ,parseOnly)
import Text.Parser.Combinators
import Control.Applicative
import Text.Parser.Permutation
import Data.Either.Validation
import           TestImport

main :: IO ()
main = hspec spec


spec :: Spec
spec = do 
  describe (nameBase 'standardize)
  undefined

goodCompany :: Text 
goodCompany = "Cowboy"

testBadCompany :: Text 
testBadCompany = "Coowboy"               
