{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


{- |
Module      :  ReportTemplate.InternalSpec
Description :  Tests for ReportTemplate Generation
Copyright   :  Plow Technologies LLC
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable



-}

module ReportTemplate.InternalSpec (main,spec) where

import           Control.Applicative             ((<$>), (<*>))
import           Control.Lens
import           Data.Aeson                      (Value (..), toJSON)
import           Data.Maybe                      (catMaybes)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Time
import           Generators
import           Kiosk.Backend.Data.DataTemplate
import           Kiosk.Backend.Form
import           Language.Haskell.TH
import           Mocks.Primitive.Generators      (GeneratorType (..),
                                                  generateInts)
import           ReportTemplate.Internal
import           Test.Hspec
import           Test.QuickCheck

makeLenses ''Company

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe (nameBase 'renderReport) $ do
   it "should Build a report from data sources contexts and templates" $ do
      True `shouldBe` True


testFormTemplate = take 1 <$> (generate . generateForm $ Static)
testDataTemplateEntries = take 1 <$> (generate . generateDataTemplate $ Static)

data ReportContext = ReportContext { currentTime :: UTCTime }

type TestReportTemplate = ReportTemplate ReportContext Form Value DataTemplate Value
type TestPreambleTemplate = [(ReportPreambleLabel, ReportContext -> Form -> Value)]
type TestRowTemplate = [(ReportRowLabel, ReportContext -> DataTemplate -> Value)]


buildTestReportTemplate :: TestReportTemplate
buildTestReportTemplate = buildReportTemplate preambleTemplate rowTemplate

preambleTemplate :: TestPreambleTemplate
preambleTemplate = [("Company Name", const getFormConstant)]

rowTemplate :: TestRowTemplate
rowTemplate = [("The First Item ",getItem0Text)
              , ("The Second Item", getItem1Text)
              , ("Time" , getItemTime)]
  where
   getItem0Text _ dt = getValFromDataTemplate "item 0" dt
   getItem1Text _ dt = getValFromDataTemplate "item 1" dt
   getItemTime t _ = toJSON.show.currentTime $ t
getFormConstant :: Form -> Value
getFormConstant form = toJSON $ form ^. getCompany.getCompanyText

getValFromDataTemplate ::
  Text -> DataTemplate -> Value
getValFromDataTemplate l dt = toJSON . catMaybes.
                              (fmap (getItemMatchingLabel l)) .
                              templateItems $ dt

getItemMatchingLabel l (TemplateItem lbl inVal)
 |l == lbl = Just inVal
 |otherwise = Nothing
