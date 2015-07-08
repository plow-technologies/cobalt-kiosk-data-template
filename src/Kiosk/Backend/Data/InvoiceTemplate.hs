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
--
--
--------------------------------------------------------------------------------

module Kiosk.Backend.Data.InvoiceTemplate where
import           Control.Applicative               ((<$>), (<*>))
import           Control.Lens
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
import           ReportTemplate.Internal
import           Text.Read                         (readMaybe)

--------------------------------------------------------------------------------
-- $setup
--
-- >>> :set -XOverloadedStrings
--
-- >>> import qualified Data.Aeson as Aeson
-- >>> import qualified Data.ByteString.Lazy as ByteString
-- >>> import qualified Data.Maybe as Maybe
--
-- >>> :{
--   let dataTemplateObject =
--         ByteString.intercalate
--           ","
--           ["{\"Amount\":\"50\""
--           , "\"Truck_#\":\"2\""
--           , "\"Type_of_Water_Hauled\":\"Fresh Water\""
--           , "\"Name_of_Lease\":\"Harrell\""
--           , "\"Customer_Ticket_#\":\"A-1183\""
--           , "\"Water_Hauling_Permit_#\":\"\""
--           , "\"Date\":\"02/03/2015\""
--           , "\"Name_of_Lease_Operator\":\"3R Oil Corporation\""
--           , "\"Water Hauling Company\":\"Mitchell Tank Truck Services\""
--           , "\"Driver_Signature\":\"\""
--           , "\"Time_In\":\"1:33 PM\"}"
--           ]
-- :}
--
-- >>> let dataTemplate = Maybe.fromJust (Aeson.decode dataTemplateObject)


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
-- >>> let someSpecificRetriever labels = genericDataTemplateRetrieval  template construct  labels
-- >>>     template = Text.concat
-- >>>     construct = LineElementDescription

genericDataTemplateRetrieval :: forall intermediate final.
                                      ([Text] -> intermediate) ->
                                      (intermediate -> final )-> [Text] -> DataTemplate -> final
genericDataTemplateRetrieval templateFcn ouputConstructor  txts dte = ouputConstructor . templateFcn $ targetTextList
  where
     inputTextLens = _InputTypeText.getInputText
     targetTextList :: [Text]
     targetTextList = fromMaybe "" <$> (getInputTypeByLabel inputTextLens <$>
                                        txts <*> [dte])


assembleSalesLineFromList :: [LineElement] -> Either Text Line
assembleSalesLineFromList elementList = makeSureRequiredFieldsArePresent =<<
                                        splitSalesLineDetailAndConstruct elementList =<<
                                        constructMinLine elementList
  where
    initialSalesItemLine :: Line
    initialSalesItemLine = Line Nothing Nothing Nothing 0.0 Nothing "" Nothing Nothing Nothing Nothing Nothing

    makeSureRequiredFieldsArePresent  :: Line -> Either Text Line
    makeSureRequiredFieldsArePresent notSureWhatToPutHere = Right notSureWhatToPutHere

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
  ReportTemplate InvoiceContext Form Invoice DataTemplate LineElement





--------------------------------------------------------------------------------
-- * QuickBooks invoice report context
--------------------------------------------------------------------------------

-- | A QuickBooks invoice report context.

data InvoiceContext = InvoiceContext
  { invoiceContextTime :: UTCTime
--  , invoiceTemplate     :: InvoiceTemplate
--  , invoiceLineTemplate :: InvoiceLineTemplate
  }



--------------------------------------------------------------------------------
-- * Create a QuickBooks invoice
-- create Lines and Invoices from the Report
--------------------------------------------------------------------------------

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
-- >>> defaultDataTemplateToLineDescription dataTemplate
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





