{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

--------------------------------------------------------------------------------
-- |
--
-- Module      : Kiosk.Backend.Data.InvoiceTemplate
-- Description :
-- Copyright   :
-- License     :
-- Maintainer  :
-- Stability   :
-- Portability :
--
--------------------------------------------------------------------------------

module Kiosk.Backend.Data.InvoiceTemplate where
import           Control.Applicative               ((<$>), (<*>))
import           Control.Lens
import           Data.HashMap.Lazy                 (HashMap)
import qualified Data.HashMap.Lazy                 as HM
import qualified Data.Map                          as M
import           Data.Map.Lazy                     (Map)
import           Data.Maybe                        (fromMaybe)
import           Data.Monoid
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Data.Time                         (UTCTime)
import           Kiosk.Backend.Data
import           Kiosk.Backend.Data.ReportTemplate
import           Kiosk.Backend.Form
import           QuickBooks
import           ReportTemplate.Report
import           Text.Read                         (readMaybe)

--------------------------------------------------------------------------------
-- $setup
--
--


--------------------------------------------------------------------------------
-- QuickBooks invoice report
--------------------------------------------------------------------------------

-- | Sum type to build various pieces of a Quickbooks
data LineElement =  LineElementId LineId
                  | LineElementNum Double
                  | LineElementDescription Text
                  | LineElementAmount Double
                  | LineElementLinkedTxn [LinkedTxn]
                  | LineElementCustomField CustomField
                  | LineElementType LineDetailType
                  | LineElementError Text
       deriving (Eq,Show)

data LineDetailType = LineDetailTypeDescriptionLineDetail DescriptionLineDetail
                    | LineDetailTypeDiscountLineDetail DiscountLineDetail
                    | LineDetailTypeSalesItemLineDetail [SalesItemLineDetailElement]
                    | LineDetailTypeSubTotalLineDetail SubTotalLineDetail
  deriving (Eq,Show)

data SalesItemLineDetailElement = SalesItemLineDetailElementItemRef ItemRef
                                | SalesItemLineDetailElementClassRef ClassRef
                                | SalesItemLineDetailElementUnitPrice Double
                                | SalesItemLineDetailElementRatePercent Double
                                | SalesItemLineDetailElementPriceLevelRef PriceLevelRef
                                | SalesItemLineDetailElementMarkupInfo Text
                                | SalesItemLineDetailElementQty Double
                                | SalesItemLineDetailElementTaxCodeRef TaxCodeRef
                                | SalesItemLineDetailElementServiceData Text
                                | SalesItemLineDetailElementTaxInclusiveAmt Double
  deriving (Eq,Show)

makePrisms ''LineElement
makePrisms ''LineDetailType
makePrisms ''SalesItemLineDetailElement


-- \ generic element constructor
-- | The Labels given in the [Text] list are
-- used to retrieve Text
-- Then two connection functions are ran to complete the transform
-- from a List of Retrieved Text to a final type

genericDataTemplateRetrieval :: forall intermediate final.
                                      ([Maybe Text] -> intermediate) ->
                                      (intermediate -> final )-> [Text] -> DataTemplate -> final
genericDataTemplateRetrieval templateFcn ouputConstructor  txts dte = ouputConstructor . templateFcn $ targetTextList
  where
     inputTextLens = _InputTypeText.getInputText
     targetTextList :: [Maybe Text]
     targetTextList = getInputTypeByLabel inputTextLens <$>
                       txts <*> [dte]



assembleSalesLineFromList :: [LineElement] -> Either Text Line
assembleSalesLineFromList elementList = buildAmountUp  =<<
                                        splitSalesLineDetailAndConstruct elementList =<<
                                        constructMinLine elementList
  where
    initialSalesItemLine :: Line
    initialSalesItemLine = Line Nothing Nothing Nothing Nothing Nothing "SalesItemLineDetail" Nothing Nothing Nothing Nothing Nothing

    buildAmountUp  :: Line -> Either Text Line
    buildAmountUp almostFinalLine = do
        sild <-  maybe (Left "detail not found") Right (lineSalesItemLineDetail almostFinalLine)
        amt <- calculateAmount sild
        return $ almostFinalLine {lineAmount = Just amt}

    splitSalesLineDetailAndConstruct :: [LineElement] -> Line -> Either Text Line
    splitSalesLineDetailAndConstruct elementList' initialLineElement = foldr foldrOverFilteredList (Right initialLineElement) elementList'

    constructMinLine :: [LineElement] -> Either Text Line
    constructMinLine elementList' = (\salesLineList customFieldList -> initialSalesItemLine {lineSalesItemLineDetail = Just salesLineList
                                                                                            , lineCustomField = Just customFieldList}) <$>
                                    (assembleSalesItemLineDetailFromList .
                                     toListOf (folded._LineElementType._LineDetailTypeSalesItemLineDetail.folded) $ elementList') <*>
                                    (Right . toListOf (folded._LineElementCustomField) $ elementList' )
    foldrOverFilteredList  :: LineElement -> Either Text Line -> Either Text Line
    foldrOverFilteredList  (LineElementId e) eitherLine = (\line -> line {lineId = Just e}) <$> eitherLine
    foldrOverFilteredList  (LineElementNum e)  eitherLine = (\line -> line {lineLineNum = Just e}) <$> eitherLine
    foldrOverFilteredList  (LineElementDescription e) eitherLine = (\line ->line {lineDescription = Just e}) <$> eitherLine
    foldrOverFilteredList  (LineElementAmount _) lineReturnedThisHasToBeCalculated = lineReturnedThisHasToBeCalculated
    foldrOverFilteredList  (LineElementLinkedTxn e) eitherLine = (\line -> line {lineLinkedTxn = Just e}) <$> eitherLine
    foldrOverFilteredList  (LineElementCustomField _) lineNotUsedBecauseCustomFieldsAreFilledElsewhere =
                                                      lineNotUsedBecauseCustomFieldsAreFilledElsewhere
    foldrOverFilteredList  (LineElementType _) lineWhichShouldntBeCalled =
                                               lineWhichShouldntBeCalled -- This is dealt with elsewhere
    foldrOverFilteredList  (LineElementError t) _ = Left t

-- | None of the fields are marked as required in a sales Item line detail soooo the easiest fold element is an empty one
-- ItemRef is actually requered by the standard however so its existence is checked at the last step
-- Items later in the list have precedence over items earlier in it.

calculateAmount :: SalesItemLineDetail -> Either Text Double
calculateAmount sild = calculatePrice
 where
  calculatePrice = maybe errMsg Right maybeCalculatePrice
  errMsg = Left $ "Price or Qty missing Price:" <>
           (Text.pack.show . salesItemLineDetailUnitPrice $ sild) <>
           " Qty:" <> (Text.pack . show . salesItemLineDetailQty $ sild)
  maybeCalculatePrice = (*) <$> salesItemLineDetailQty sild
                         <*>  salesItemLineDetailUnitPrice  sild

assembleSalesItemLineDetailFromList :: [SalesItemLineDetailElement] -> Either Text SalesItemLineDetail
assembleSalesItemLineDetailFromList salesItemLineDetailElementList  = makeSureItemRefExists .
                                                                      foldrSalesItemLineDetail $ salesItemLineDetailElementList
  where
     makeSureItemRefExists :: SalesItemLineDetail -> Either Text SalesItemLineDetail
     makeSureItemRefExists  salesItemLineDetail' = maybe  errNoItemRef (const $ Right salesItemLineDetail') .
                                                                       salesItemLineDetailItemRef $ salesItemLineDetail'
     errNoItemRef = Left "Missing 'LineDetailItemRef' in SalesItemLineDetail"
     initialSalesItemLineDetail :: SalesItemLineDetail
     initialSalesItemLineDetail = SalesItemLineDetail Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
     foldrSalesItemLineDetail :: [SalesItemLineDetailElement] -> SalesItemLineDetail
     foldrSalesItemLineDetail = foldr constructSalesItemLineDetailInFoldr initialSalesItemLineDetail

-- | Pattern Matcher for
constructSalesItemLineDetailInFoldr :: SalesItemLineDetailElement -> SalesItemLineDetail -> SalesItemLineDetail
constructSalesItemLineDetailInFoldr (SalesItemLineDetailElementItemRef e)          sild = sild{salesItemLineDetailItemRef = Just e}
constructSalesItemLineDetailInFoldr (SalesItemLineDetailElementClassRef e)         sild = sild{salesItemLineDetailClassRef = Just e}
constructSalesItemLineDetailInFoldr (SalesItemLineDetailElementUnitPrice e)        sild = sild{salesItemLineDetailUnitPrice = Just e}
constructSalesItemLineDetailInFoldr (SalesItemLineDetailElementRatePercent e)      sild = sild{salesItemLineDetailRatePercent = Just e}
constructSalesItemLineDetailInFoldr (SalesItemLineDetailElementPriceLevelRef e)    sild = sild{salesItemLineDetailPriceLevelRef = Just e}
constructSalesItemLineDetailInFoldr (SalesItemLineDetailElementMarkupInfo e)       sild = sild{salesItemLineDetailMarkupInfo = Just e}
constructSalesItemLineDetailInFoldr (SalesItemLineDetailElementQty e)               sild = sild{salesItemLineDetailQty = Just e}
constructSalesItemLineDetailInFoldr (SalesItemLineDetailElementTaxCodeRef e)       sild = sild{salesItemLineDetailTaxCodeRef = Just e}
constructSalesItemLineDetailInFoldr (SalesItemLineDetailElementServiceData e)      sild = sild{salesItemLineDetailServiceData = Just e}
constructSalesItemLineDetailInFoldr (SalesItemLineDetailElementTaxInclusiveAmt e)  sild = sild{salesItemLineDetailTaxInclusiveAmt = Just e}




-- | A QuickBooks invoice report.
type InvoiceReport =
  Report Invoice LineElement



-- | A QuickBooks invoice report template.
type InvoiceReportTemplate =
  ReportTemplate InvoiceContext Form Invoice DataTemplateEntry LineElement





--------------------------------------------------------------------------------
-- * QuickBooks invoice report context
--------------------------------------------------------------------------------

-- | A QuickBooks invoice report context.

data InvoiceContext = InvoiceContext
  {  invoiceContextTime   :: UTCTime
    ,invoiceCompanyRefMap :: HashMap Text CustomerRef
    ,invoiceItemRefMap    :: HashMap Text ItemRef
--  , invoiceTemplate     :: InvoiceTemplate
--  , invoiceLineTemplate :: InvoiceLineTemplate
  }



--------------------------------------------------------------------------------
-- * Create a QuickBooks invoice
-- create Lines and Invoices from the Report
--------------------------------------------------------------------------------

-- There are lots of errors from which I want to recover.  For this reason, I wanted to return partial errors up the chain.
-- That is why the type ends up being kind of crazy at the top.  It might be simpler but it isn't now so oh well


reportToInvoice ::
  Report Invoice LineElement -> (Text, Maybe Invoice)
reportToInvoice r = maybe (topLevelErrorMessage, Nothing) (\invoice'-> (errorsFromReport, Just invoice')) $
                    (\invoiceFromReport -> invoiceFromReport {invoiceLine=linesFromReport}) <$>
                    maybeInvoiceFromReport
 where
   topLevelErrorMessage = "unableToretrieveInvoice, line errors: " <> errorsFromReport
   maybeInvoiceFromReport :: Maybe Invoice
   maybeInvoiceFromReport = r ^? reportPreamble.preambleValue.folded._2
   lineElementsFromReport :: Maybe (ReportTableRowStyle LineElement)
   lineElementsFromReport = r ^? (reportRows. _ReportTableRowIndex._2)
   (errorsFromReport,linesFromReport) = fromMaybe ("Missing Report Rows", []) $  makeReportLines <$>
                                        lineElementsFromReport

-- |1. the errors in lines 2. the Values that are correct
makeReportLines :: ReportTableRowStyle LineElement
                -> (Text,[Line])
makeReportLines = convertLineElementMapToLineMap . reportRowLineFoldr
  where
    groupErrorsAssembleLines :: [LineElement] -> (Text,[Line]) -> (Text,[Line])
    groupErrorsAssembleLines leLst (errs,vals)  = either (\err -> (err<> " " <>  " " <>errs, vals))
                                                         (\val -> (errs,val:vals)) . assembleSalesLineFromList $ leLst
    convertLineElementMapToLineMap :: Map RowNumber [LineElement] -> (Text, [Line])
    convertLineElementMapToLineMap = M.foldr groupErrorsAssembleLines ("",[])
    reportRowLineFoldr :: ReportTableRowStyle LineElement -> Map RowNumber [LineElement]
    reportRowLineFoldr = foldrTableByRowWithIndex lineListMapConstructor M.empty
    lineListMapConstructor (r,_) i map'  = M.insertWith (\nv ov -> nv ++ ov) r [i] map'

--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

-- |

dataTemplateItems
  :: DataTemplate
  -> [(Text,InputType)]

dataTemplateItems DataTemplate{templateItems} =
  fmap (\TemplateItem{..} -> (label,templateValue)) templateItems


-- |

defaultLookupInDataTemplate
  :: DataTemplate
  -> Text
  -> Maybe InputType

defaultLookupInDataTemplate dataTemplate =
  flip lookup (dataTemplateItems dataTemplate)


-- |

defaultLookupDoubleInDataTemplate
  :: DataTemplate
  -> Text
  -> Maybe Double

defaultLookupDoubleInDataTemplate dataTemplate label =
  let
    maybeTextValue =
      defaultLookupTextInDataTemplate dataTemplate label
  in
    fmap Text.unpack maybeTextValue >>= readMaybe


-- |

defaultLookupTextInDataTemplate
  :: DataTemplate
  -> Text
  -> Maybe Text

defaultLookupTextInDataTemplate dataTemplate label =
  case defaultLookupInDataTemplate dataTemplate label of
    Just (InputTypeText textValue) ->
      Just (_getInputText textValue)
    _ ->
      Nothing





-- |
--

-- "02/03/2015 Barrels of fresh water disposed of. Ticket #A-1183"

defaultDataTemplateToLineDescription :: DataTemplate -> Text
defaultDataTemplateToLineDescription dataTemplate =
  Text.intercalate
    " "
    [ dateDescription
    , waterDescription
    , ticketDescription
    ]
  where
    dateDescription :: Text
    dateDescription =
      fromMaybe
        ""
        (defaultLookupTextInDataTemplate dataTemplate "Date")

    ticketDescription :: Text
    ticketDescription =
      Text.append "Ticket #" ticketNumber

    ticketNumber :: Text
    ticketNumber =
      fromMaybe
        ""
        (defaultLookupTextInDataTemplate dataTemplate "Customer_Ticket_#")

    water :: Text
    water =
      fromMaybe
        ""
        (defaultLookupTextInDataTemplate dataTemplate "Type_of_Water_Hauled")

    waterDescription :: Text
    waterDescription =
      Text.concat ["Barrels of ", Text.toLower water, " disposed of."]





