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
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as M
import           Data.Maybe                      (catMaybes)
import           Data.Monoid                     ((<>))
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
makeLenses ''Report
makePrisms ''ReportTable
makeLenses ''ReportTableRowStyle
makeLenses ''ReportPreamble

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe (nameBase 'renderReport) $ do
   it "should Build a report from data sources contexts and templates" $ do
      True `shouldBe` True


testFormTemplate = take 1 <$> (generate . generateForm $ Static)
testDataTemplateEntries i = take i <$> (generate . generateDataTemplate $ Static)

data ReportContext = ReportContext { currentTime :: UTCTime }

type TestReportTemplate = ReportTemplate ReportContext Form Value DataTemplate Value
type TestPreambleTemplate = [(ReportPreambleLabel, ReportContext -> Form -> Value)]
type TestRowTemplate = [(ReportRowLabel, ReportContext -> DataTemplate -> Value)]


-- | this is a rendered report
testReport :: Int -> IO (Report Value Value)
testReport i = do
   (oneForm:_) <- testFormTemplate
   dtes <- testDataTemplateEntries i
   tm   <- ReportContext <$> getCurrentTime
   return . renderReport testReportTemplate tm oneForm $ dtes

-- | This is a test Report Template which should constructo a Report
testReportTemplate :: TestReportTemplate
testReportTemplate = buildReportTemplate preambleTemplate rowTemplate

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


testBuildADocument report = "Welcome to another fine report \n" <>
                            renderPreamble
                            <> "\n\n\n"
                            <> renderRowByRow
  where
   renderPreamble = T.unwords . fmap renderPreamblePair $ report ^. (reportPreamble.preambleValue)
   renderPreamblePair (l,v) = (T.pack l)<> ": " <> (T.pack . show $ l)
   renderRowByRow = M.foldrWithKey renderRowString "" $ report ^. (reportRows .
                                                                   _ReportTableRowIndex.
                                                                   rowMap)


renderRowString = undefined
