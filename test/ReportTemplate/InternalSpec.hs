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
import           Data.Aeson                      (Value (..), encode, toJSON)
import           Data.Text                       (Text)
import qualified Data.Text                       as T

import           Data.ByteString.Lazy.Char8      (ByteString)
import qualified Data.ByteString.Lazy.Char8      as B
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as M
import           Data.Maybe                      (catMaybes)
import           Data.Monoid                     ((<>))
import           Data.Time
import           Generators
import           Kiosk.Backend.Data.DataTemplate
import           Kiosk.Backend.Data.InvoiceTemplate
import           Kiosk.Backend.Form
import           Language.Haskell.TH

import           Mocks.Primitive.Generators      (GeneratorType (..),
                                                  generateInts)
import           ReportTemplate.Internal
import           System.Locale                   (defaultTimeLocale)
import           Test.Hspec
import           Test.QuickCheck

makeLenses ''Company
makeLenses ''Report
makePrisms ''ReportTable
makeLenses ''ReportTableRowStyle
makeLenses ''ReportPreamble
makePrisms ''InputType
makeLenses ''InputText

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe (nameBase 'renderReport) $
    it "should Build a report from data sources contexts and templates" $ buildReport
  describe (nameBase 'buildInvoice) $  
    it "should build a quickbooks invoice from contexts and templates"  $ buildInvoice

buildReport = do
  let i = 7
  (reportTemplate,report) <- testReport i
  let mapSize = M.size $ report ^. (reportRows .
                                    _ReportTableRowIndex._2.
                                    rowMap)
      rowTemplateLength = length rowTemplate                      
  mapSize `shouldBe` (i * rowTemplateLength)
  (M.size . rowTransformMap.reportRowsTemplate $ reportTemplate ) `shouldBe` rowTemplateLength

buildInvoice :: Expectation
buildInvoice = do
  let i = 7
  dataTemplates <- testDataTemplate i
  let invoice = renderInvoice undefined
  True `shouldBe` True

testFormTemplate :: IO [Form]
testFormTemplate = take 1 <$> (generate . generateForm $ Static)

testDataTemplate :: Int -> IO [DataTemplate]
testDataTemplate i = do
          lst <- (generate .listOf. generateDataTemplate $ Static)
          return $ concat . take i $ lst

data ReportContext = ReportContext { currentTime :: UTCTime }

type TestReportTemplate = ReportTemplate ReportContext Form Value DataTemplate ByteString
type TestPreambleTemplate = [(ReportPreambleLabel, ReportContext -> Form -> Value)]
type TestRowTemplate = [(ReportRowLabel, ReportContext -> DataTemplate -> ByteString)]


-- | this is a rendered report
testReport :: Int -> IO (TestReportTemplate , Report Value ByteString)
testReport i = do
   (oneForm:_) <- testFormTemplate
   dtes <- testDataTemplate i
   tm   <- ReportContext <$> getCurrentTime
   return ( testReportTemplate, renderReport testReportTemplate tm oneForm dtes)

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
   getItemTime t _ = B.pack.formatTime defaultTimeLocale "%c" . currentTime $ t

getFormConstant :: Form -> Value
getFormConstant form = toJSON $ form ^. getCompany.getCompanyText

getValFromDataTemplate ::
  Text -> DataTemplate -> ByteString
getValFromDataTemplate l dt = B.unwords . catMaybes.
                              (fmap (getItemMatchingLabel l)) .
                              templateItems $ dt

getItemMatchingLabel l (TemplateItem lbl inVal)
 |l == lbl = B.pack . T.unpack <$> inVal ^? (_InputTypeText.getInputText)
 |otherwise = Nothing


testBuildADocument report = "Welcome to another fine report \n" <>
                            renderPreamble
                            <> "\n\n\n"
                            <> renderRowByRow
  where
   renderPreamble = B.unwords . fmap renderPreamblePair $ report ^. (reportPreamble.preambleValue)
   renderPreamblePair (l,v) = (B.pack l) <> ": " <> (encode l)
   renderRowByRow = snd $ M.foldrWithKey renderRowString (1,"") $ report ^. (reportRows .
                                                                                   _ReportTableRowIndex._2.
                                                                                   rowMap)

renderRowString :: (RowNumber,String) -> ByteString ->(RowNumber,ByteString) -> (RowNumber,ByteString)
renderRowString (idx,lbl) val (itarget,txt)
  | idx == itarget = (itarget, txt <>  val <> ",")
  | otherwise = (idx, (B.pack . show $ idx) <> txt <> "\n"  <>  val <> ",")
