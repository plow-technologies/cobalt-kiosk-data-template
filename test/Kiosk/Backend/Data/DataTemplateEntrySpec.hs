{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Data.DataTemplateEntrySpec where

-- import           Control.Applicative             ((<$>))
-- import           Control.Arrow                   ((***))
-- import           Data.Aeson                      (Value (..), decode,
--                                                   eitherDecode, encode, toJSON)
-- import           Data.ByteString.Lazy.Internal   (ByteString)
-- import           Data.Either                     (rights)

-- import qualified Data.HashMap.Strict             as HM
-- import           Data.List                       (sort)
-- import           Data.Text                       (Text)
-- import           Generators                      (GeneratorType (..), checkStaticGeneratorConsistency,
--                                                   generateDataTemplateEntry,
--                                                   generateForm)

import           Codec.Xlsx.Types                ( def
                                                 , Xlsx (..)
                                                 , Worksheet (..)
                                                 )
import           Data.Maybe                      (fromMaybe)
import           Data.List                       (nub)
import qualified Data.Map as M                   (lookup, keys)
import           Kiosk.Backend.Data.DataTemplateEntry (fromDataTemplateEntryToXlsx)
import           Test.Hspec
import           Test.QuickCheck                 (quickCheck)
import           Generators

import           Kiosk.Backend.Data              (DataTemplateEntry (..))




-- import           TestImport                      (testJSON)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Xlsx should have a cellValue corresponding to every templateEntryItem" $ undefined


noDataLostTest = quickCheck prop_no_data_loss

prop_no_data_loss :: [DataTemplateEntry] -> Bool
prop_no_data_loss xs = numDataTemplateEntries == numRowsInExcelSheet
 where
  numDataTemplateEntries = length xs
  sheet                  = fromMaybe def $ M.lookup "" (_xlSheets (fromDataTemplateEntryToXlsx xs))
  numRowsInExcelSheet    = length . nub . (fmap (snd)) . M.keys  $  _wsCells sheet

                             



