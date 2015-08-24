{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{- |
Module      :  ReportTemplate.InternalSpec
Description :  Tests for Repo.LazyrtTemplate Generation
Copyright   :  Plow Technolog.Lazyies LLC
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable



-}

module ReportTemplate.InternalSpec (main,spec) where
import           Control.Applicative                     (pure, (<$>), (<*>))
import           Control.Lens
import           Data.Aeson                              (Result, Value (..),
                                                          encode, toJSON)
import qualified Data.Aeson                              as A

import           Data.Text                               (Text)
import qualified Data.Text                               as T
import           Text.Read                               (readEither)

import           Data.ByteString.Lazy.Char8              (ByteString)
import qualified Data.ByteString.Lazy.Char8              as B
import           Data.HashMap.Lazy                       (HashMap)
import qualified Data.HashMap.Lazy                       as HM
import qualified Data.Map.Strict                         as M

import           Data.Maybe                              (catMaybes, fromMaybe,
                                                          isJust)
import           Data.Monoid                             ((<>))

import           Data.Time
import           Generators

import           Kiosk.Backend.Data.DataTemplate
import           Kiosk.Backend.Data.DataTemplateEntry
import           Kiosk.Backend.Data.DataTemplateEntryKey
import           Kiosk.Backend.Data.InvoiceTemplate
import           Kiosk.Backend.Form

import           Language.Haskell.TH
import           Mocks.Primitive.Generators              (GeneratorType (..),
                                                          generateInts)

import           QuickBooks
import           QuickBooks.Types
import           ReportTemplate.Internal
import           Test.Hspec
import           Test.QuickCheck
import           TestImport

makeLenses ''Company
makeLenses ''Report
makePrisms ''ReportTable
makeLenses ''ReportTableRowStyle
makeLenses ''ReportPreamble
makePrisms ''InputType
makeLenses ''InputText
makeLenses ''DataTemplateEntryKey
makeLenses ''TicketId
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


(Just goldenDataTemplate) = A.decode newDataTemplateByteString :: Maybe DataTemplateEntry

buildInvoice :: Expectation
buildInvoice = do
  let i = 7
  (invoiceReportTemplate,invoiceReport) <- testInvoiceReport
  let (errs,maybeReport) = reportToInvoice invoiceReport
  errs `shouldBe` T.empty
  (isJust maybeReport) `shouldBe` True


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


testSendInvoice = do
 let oauth = OAuthToken "qyprdB9yMcB3a5FeDA8h8AcfCbYkwhIDMBvYnGRQNewbt4KQ" "Ll9zlcjp7OIp9Xk1lyn2jadeenNhhHKxjAOVpGBM"
 (_,ir) <- testInvoiceReport
 let (errs,(Just inv)) = reportToInvoice ir
 print . A.encode $ inv
 createInvoice oauth inv


testInvoiceTemplate :: InvoiceReportTemplate
testInvoiceTemplate = buildReportTemplate [("Company Reference", const createInvoiceFromForm)] invoiceLineTemplate
  where
    createInvoiceFromForm form = defaultInvoice [emptySalesItemLine] (Reference Nothing Nothing "21")
    invoiceLineTemplate = [("Product/Service", const getCompanyName)
                          ,("Description",const getLineDescription)
                          ,("ItemRef",\_ _ -> itemRef)
                          ,("Qty", const getQty)
                          ,("Amt", \_ _ ->  getPrice)]
    itemRef = LineElementType . LineDetailTypeSalesItemLineDetail
                                . (: []) . SalesItemLineDetailElementItemRef . Reference Nothing Nothing $ "2"
    getCompanyName :: DataTemplateEntry -> LineElement
    getCompanyName  = genericDataTemplateRetrieval cleanCompanyName
                                                   customProductServiceField
                                                   ["Water Hauling Company"] . _dataTemplateEntryValue
    cleanCompanyName = T.concat . catMaybes
    customProductServiceField val = LineElementCustomField
                                      CustomField { customFieldDefinitionId = "ProductOrService"
                                                     , customFieldName = "Product/Service"
                                                     , customFieldType = StringType
                                                     , customFieldStringValue = Just val
                                                     , customFieldBooleanValue = Nothing
                                                     , customFieldDateValue = Nothing
                                                     , customFieldNumberValue = Nothing}

    getLineDescription dte = over _LineElementDescription (appendTicketNumber dte) (genericDataTemplateRetrieval assembleDescription LineElementDescription ["Type_of_Water_Hauled"
                                                                                                                                                           ,   "Date"
                                                                                                                                                           ,    "Time_In"] . _dataTemplateEntryValue $ dte)
    formatTicketIdCorrectly ipadNumber ticketNumber =  (T.pack.show $ ipadNumber ) <> "-" <> (T.pack.show $ ticketNumber)
    appendTicketNumber dte txt = txt <> " ticket# " <> (dte ^. dataTemplateEntryKey.getTicketId.getTicketIdPair.runGetter (formatTicketIdCorrectly
                                                                                                                          <$> Getter _2 <*> Getter _1 ))

    assembleDescription [mayType, mayDate, mayTimeIn] = fromMaybe "date not found" mayDate <>
                                                                     " Barrels of " <> fromMaybe "water type not found" mayType <> " disposed of"

    assembleDescription strLst = "Error retrieving description for this line" <> (T.pack . show $  strLst)
    getQty = genericDataTemplateRetrieval convertQtyToDouble (makeLineElement SalesItemLineDetailElementQty) ["Amount"]  . _dataTemplateEntryValue
    getPrice = makeLineElement SalesItemLineDetailElementUnitPrice .Right $  0.4
    convertQtyToDouble :: [Maybe Text] -> Either Text Double
    convertQtyToDouble doubleTextList = over _Left (packAndLog . fmap  (fromMaybe "No incoming text" ) $  doubleTextList) (readEither . T.unpack . T.concat . fmap (fromMaybe "0.8") $ doubleTextList )
    packAndLog doubleText = const $ "Error reading text as number, recieved (" <> (T.concat doubleText) <> ")"
    makeLineElement salesItemConstructor = either LineElementError (  LineElementType
                                                                    . LineDetailTypeSalesItemLineDetail
                                                                    . (: []) . salesItemConstructor)

testInvoiceReport ::  IO (InvoiceReportTemplate, InvoiceReport)
testInvoiceReport = do
  (oneForm:_) <- testFormTemplate
  tm <- InvoiceContext <$> getCurrentTime <*> pure HM.empty <*> pure HM.empty
  return (testInvoiceTemplate, renderReport testInvoiceTemplate tm oneForm [goldenDataTemplate])

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
                              fmap (getItemMatchingLabel l) .
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


renderRowString :: (RowNumber,String) -> ByteString
                     -> (RowNumber,ByteString) -> (RowNumber,ByteString)
renderRowString (idx,lbl) val' (itarget,txt)
  | idx == itarget = (itarget, txt <>  val' <> ",")
  | otherwise = (idx, (B.pack . show $ idx) <> txt <> "\n"  <>  val' <> ",")
