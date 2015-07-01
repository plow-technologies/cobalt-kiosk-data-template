{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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


import Kiosk.Backend.Data
import Kiosk.Backend.Form
import QuickBooks
import ReportTemplate.Internal

import Control.Applicative ((<*>))
import Control.Applicative (pure)
import Data.Maybe (catMaybes,fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Text.Read (readMaybe)


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

-- | A QuickBooks invoice report.

type InvoiceReport =
  Report Invoice Line


-- | A QuickBooks invoice report template.

type InvoiceReportTemplate =
  ReportTemplate InvoiceContext Form Invoice DataTemplate Line


-- |

buildInvoiceReport
  :: InvoiceReportTemplate
  -> InvoiceContext
  -> Form
  -> [DataTemplate]
  -> InvoiceReport

buildInvoiceReport  =
  renderReport


-- |

renderInvoice
  :: InvoiceReport
  -> Invoice

renderInvoice _ =
  undefined


--------------------------------------------------------------------------------
-- * QuickBooks invoice report context
--------------------------------------------------------------------------------

-- | A QuickBooks invoice report context.

data InvoiceContext = InvoiceContext
  { invoiceContextTime  :: UTCTime
  , invoiceTemplate     :: InvoiceTemplate
  , invoiceLineTemplate :: InvoiceLineTemplate
  }


data InvoiceTemplate = InvoiceTemplate
  { maybeCustomerName                 :: Maybe Text
  , customerValue                     :: Text
  , dataTemplateToInvoiceCustomField1 :: Maybe (Form -> CustomField)
  , dataTemplateToInvoiceCustomField2 :: Maybe (Form -> CustomField)
  , dataTemplateToInvoiceCustomField3 :: Maybe (Form -> CustomField)
  , maybeInvoiceDate                  :: Maybe Text
  , maybeInvoiceDueDate               :: Maybe Text
  }


data InvoiceLineTemplate = InvoiceLineTemplate
  { dataTemplateToLineCustomField1 :: Maybe (DataTemplate -> CustomField)
  , dataTemplateToLineCustomField2 :: Maybe (DataTemplate -> CustomField)
  , dataTemplateToLineCustomField3 :: Maybe (DataTemplate -> CustomField)
  , dataTemplateToLineDescription  :: Maybe (DataTemplate -> Text)
  , invoiceLineDetailTemplate      :: InvoiceLineDetailTemplate
  }


data InvoiceLineDetailTemplate = InvoiceLineDetailTemplate
  { maybeItemName  :: Maybe Text
  , itemValue      :: Text
  , itemUnitPrice  :: Double
  , quantityLabel  :: Text
  , unitPriceLabel :: Text
  }


--------------------------------------------------------------------------------
-- * Create a QuickBooks invoice
--------------------------------------------------------------------------------

-- ** Create an invoice given a form and data templates

-- |

formAndDataTemplatesToInvoice
  :: InvoiceContext
  -> Form
  -> [DataTemplate]
  -> Invoice

formAndDataTemplatesToInvoice InvoiceContext{..} form dataTemplates =
  invoice { invoiceLine = invoiceLines }
  where
    invoice :: Invoice
    invoice =
      formToInvoice invoiceTemplate form

    invoiceLines =
      dataTemplatesToLines invoiceLineTemplate dataTemplates


-- ** Create an invoice given a form

-- |

formToInvoice
  :: InvoiceTemplate
  -> Form
  -> Invoice

formToInvoice InvoiceTemplate{..} form =
  (defaultInvoice [] customerRef)
    { invoiceCustomField = maybeCustomFields
    , invoiceTxnDate     = maybeInvoiceDate
    , invoiceDueDate     = maybeInvoiceDueDate
    }
  where
    customerRef :: CustomerRef
    customerRef =
      (reference customerValue) { referenceName = maybeCustomerName }

    maybeCustomFields :: Maybe [CustomField]
    maybeCustomFields =
      if null customFields then Nothing else Just customFields
      where
        customFields =
          catMaybes
            [ dataTemplateToInvoiceCustomField1 <*> Just form
            , dataTemplateToInvoiceCustomField2 <*> Just form
            , dataTemplateToInvoiceCustomField3 <*> Just form
            ]


-- ** Create a line given a data template

-- |

dataTemplatesToLines
  :: InvoiceLineTemplate
  -> [DataTemplate]
  -> [Line]

dataTemplatesToLines invoiceLineTemplate =
  fmap (dataTemplateToLine invoiceLineTemplate)


-- |

dataTemplateToLine
  :: InvoiceLineTemplate
  -> DataTemplate
  -> Line

dataTemplateToLine =
  dataTemplateToSalesItemLine


-- |

dataTemplateToSalesItemLine
  :: InvoiceLineTemplate
  -> DataTemplate
  -> Line

dataTemplateToSalesItemLine InvoiceLineTemplate{..} dataTemplate =
  (salesItemLine amount detail)
    { lineCustomField = maybeCustomFields
    , lineDescription = maybeDescription
    }
  where
    amount :: Double
    amount =
      fromMaybe 0.0 maybeAmount

    maybeAmount :: Maybe Double
    maybeAmount =
      pure (*) <*> salesItemLineDetailUnitPrice detail
               <*> salesItemLineDetailQty detail

    detail :: SalesItemLineDetail
    detail =
      dataTemplateToSalesItemLineDetail invoiceLineDetailTemplate dataTemplate

    maybeCustomFields :: Maybe [CustomField]
    maybeCustomFields =
      if null customFields then Nothing else Just customFields
      where
        customFields =
          catMaybes
          [ dataTemplateToLineCustomField1 <*> Just dataTemplate
          , dataTemplateToLineCustomField2 <*> Just dataTemplate
          , dataTemplateToLineCustomField3 <*> Just dataTemplate
          ]

    maybeDescription :: Maybe Text
    maybeDescription =
      dataTemplateToLineDescription <*> Just dataTemplate


-- |

dataTemplateToSalesItemLineDetail
  :: InvoiceLineDetailTemplate
  -> DataTemplate
  -> SalesItemLineDetail

dataTemplateToSalesItemLineDetail InvoiceLineDetailTemplate{..} dataTemplate =
  (salesItemLineDetail itemRef)
    { salesItemLineDetailUnitPrice   = maybeUnitPrice
    , salesItemLineDetailQty         = maybeQuantity
    }
  where
    itemRef :: ItemRef
    itemRef =
      (reference itemValue) { referenceName = maybeItemName }

    maybeQuantity :: Maybe Double
    maybeQuantity =
      defaultLookupDoubleInDataTemplate dataTemplate quantityLabel

    maybeUnitPrice :: Maybe Double
    maybeUnitPrice =
      case defaultLookupDoubleInDataTemplate dataTemplate unitPriceLabel of
        Nothing ->
          Just itemUnitPrice

        otherItemUnitPrice ->
          otherItemUnitPrice


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


--------------------------------------------------------------------------------
-- * Example
--------------------------------------------------------------------------------

-- |

defaultInvoiceContext :: UTCTime -> InvoiceContext
defaultInvoiceContext time =
  InvoiceContext
    { invoiceContextTime  = time
    , invoiceTemplate     = defaultInvoiceTemplate ""
    , invoiceLineTemplate = defaultInvoiceLineTemplate undefined undefined
    }


-- |

defaultInvoiceTemplate :: Text -> InvoiceTemplate
defaultInvoiceTemplate customerValue =
  InvoiceTemplate
    { maybeCustomerName                 = Nothing
    , customerValue                     = customerValue
    , dataTemplateToInvoiceCustomField1 = Nothing
    , dataTemplateToInvoiceCustomField2 = Nothing
    , dataTemplateToInvoiceCustomField3 = Nothing
    , maybeInvoiceDate                  = Nothing
    , maybeInvoiceDueDate               = Nothing
    }


-- |

defaultInvoiceLineTemplate :: Text -> Double -> InvoiceLineTemplate
defaultInvoiceLineTemplate itemValue itemUnitPrice =
  InvoiceLineTemplate
    { dataTemplateToLineCustomField1 = Nothing
    , dataTemplateToLineCustomField2 = Nothing
    , dataTemplateToLineCustomField3 = Nothing
    , dataTemplateToLineDescription  = Just defaultDataTemplateToLineDescription
    , invoiceLineDetailTemplate      =
        defaultInvoiceLineDetailTemplate itemValue itemUnitPrice
    }


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


-- |

defaultInvoiceLineDetailTemplate :: Text -> Double -> InvoiceLineDetailTemplate
defaultInvoiceLineDetailTemplate itemValue itemUnitPrice =
  InvoiceLineDetailTemplate
    { maybeItemName          = Nothing
    , itemValue              = itemValue
    , itemUnitPrice          = itemUnitPrice
    , quantityLabel          = "Amount"
    , unitPriceLabel         = "Rate"
    }
