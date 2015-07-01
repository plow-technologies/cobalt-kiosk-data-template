{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

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


--------------------------------------------------------------------------------
--
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
-- *
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
  , txnDate                           :: Maybe Text
  }


defaultInvoiceTemplate :: InvoiceTemplate
defaultInvoiceTemplate =
  InvoiceTemplate
    { maybeCustomerName                 = Nothing
    , customerValue                     = ""
    , dataTemplateToInvoiceCustomField1 = Nothing
    , dataTemplateToInvoiceCustomField2 = Nothing
    , dataTemplateToInvoiceCustomField3 = Nothing
    , txnDate                           = Nothing
    }


data InvoiceLineTemplate = InvoiceLineTemplate
  { dataTemplateToLineCustomField1 :: Maybe (DataTemplate -> CustomField)
  , dataTemplateToLineCustomField2 :: Maybe (DataTemplate -> CustomField)
  , dataTemplateToLineCustomField3 :: Maybe (DataTemplate -> CustomField)
  , dataTemplateToLineDescription  :: Maybe (DataTemplate -> Text)
  , invoiceLineDetailTemplate      :: InvoiceLineDetailTemplate
  }


defaultInvoiceLineTemplate :: InvoiceLineTemplate
defaultInvoiceLineTemplate =
  InvoiceLineTemplate
    { dataTemplateToLineCustomField1 = Nothing
    , dataTemplateToLineCustomField2 = Nothing
    , dataTemplateToLineCustomField3 = Nothing
    , dataTemplateToLineDescription  = Nothing
    , invoiceLineDetailTemplate      = defaultInvoiceLineDetailTemplate
    }


data InvoiceLineDetailTemplate = InvoiceLineDetailTemplate
  { dataTemplateToItemRef  :: DataTemplate -> ItemRef
  , dataTemplateToQuantity :: DataTemplate -> Maybe Double
  , unitPrice              :: Double
  }


defaultInvoiceLineDetailTemplate :: InvoiceLineDetailTemplate
defaultInvoiceLineDetailTemplate =
  InvoiceLineDetailTemplate
    { dataTemplateToItemRef  = defaultDataTemplateToItemRef
    , dataTemplateToQuantity = defaultDataTemplateToQuantity
    , unitPrice = undefined
    }


--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- |

defaultDataTemplateToItemRef
  :: DataTemplate
  -> ItemRef

defaultDataTemplateToItemRef _ =
  undefined


-- |

defaultDataTemplateToLineDescription
  :: DataTemplate
  -> Text

defaultDataTemplateToLineDescription _ =
  "No description"


-- |

defaultDataTemplateToQuantity
  :: DataTemplate
  -> Maybe Double

defaultDataTemplateToQuantity dataTemplate =
  case defaultLookupInDataTemplate dataTemplate "Amount" of
    Just (InputTypeDouble quantity) ->
      Just (_getInputDouble quantity)

    Just (InputTypeInt quantity) ->
      Just (fromIntegral (_getInputInt quantity))

    _ ->
      Nothing


--------------------------------------------------------------------------------
-- * Create an invoice
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
    , salesItemLineDetailServiceData = maybeServiceDate
    }
  where
    itemRef :: ItemRef
    itemRef =
      dataTemplateToItemRef dataTemplate

    maybeQuantity :: Maybe Double
    maybeQuantity =
      dataTemplateToQuantity dataTemplate

    maybeServiceDate :: Maybe Text
    maybeServiceDate =
      undefined

    maybeUnitPrice :: Maybe Double
    maybeUnitPrice =
      Just unitPrice


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

defaultLookupDateInDataTemplate
  :: DataTemplate
  -> Text
  -> Maybe Text

defaultLookupDateInDataTemplate dataTemplate label =
  case defaultLookupInDataTemplate dataTemplate label of
    Just (InputTypeDate dateValue) ->
      Just (_getInputDate dateValue)
    _ ->
      Nothing


-- |

defaultLookupDoubleInDataTemplate
  :: DataTemplate
  -> Text
  -> Maybe Double

defaultLookupDoubleInDataTemplate dataTemplate label =
  case defaultLookupInDataTemplate dataTemplate label of
    Just (InputTypeDouble doubleValue) ->
      Just (_getInputDouble doubleValue)
    _ ->
      Nothing


-- |

defaultLookupIntInDataTemplate
  :: DataTemplate
  -> Text
  -> Maybe Int

defaultLookupIntInDataTemplate dataTemplate label =
  case defaultLookupInDataTemplate dataTemplate label of
    Just (InputTypeInt doubleInt) ->
      Just (_getInputInt doubleInt)
    _ ->
      Nothing


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

defaultLookupTimeInDataTemplate
  :: DataTemplate
  -> Text
  -> Maybe Text

defaultLookupTimeInDataTemplate dataTemplate label =
  case defaultLookupInDataTemplate dataTemplate label of
    Just (InputTypeTime timeValue) ->
      Just (_getInputTime timeValue)
    _ ->
      Nothing


--------------------------------------------------------------------------------
-- * Example
--------------------------------------------------------------------------------

sdInvoiceContext :: UTCTime -> InvoiceContext
sdInvoiceContext time =
  InvoiceContext
    { invoiceContextTime  = time
    , invoiceTemplate     = sdInvoiceTemplate ""
    , invoiceLineTemplate = sdInvoiceLineTemplate
    }


sdInvoiceTemplate :: Text -> InvoiceTemplate
sdInvoiceTemplate customerValue =
  InvoiceTemplate
    { maybeCustomerName                 = Nothing
    , customerValue                     = customerValue
    , dataTemplateToInvoiceCustomField1 = Nothing
    , dataTemplateToInvoiceCustomField2 = Nothing
    , dataTemplateToInvoiceCustomField3 = Nothing
    , txnDate                           = Nothing
    }


sdInvoiceLineTemplate :: InvoiceLineTemplate
sdInvoiceLineTemplate =
  InvoiceLineTemplate
    { dataTemplateToLineCustomField1 = Nothing
    , dataTemplateToLineCustomField2 = Nothing
    , dataTemplateToLineCustomField3 = Nothing
    , dataTemplateToLineDescription  = Just sdDataTemplateToLineDescription
    , invoiceLineDetailTemplate      = sdInvoiceLineDetailTemplate 2.0
    }

sdDataTemplateToLineDescription :: DataTemplate -> Text
sdDataTemplateToLineDescription dataTemplate =
  Text.concat [ticketId]
  where
    ticketId :: Text
    ticketId =
      fromMaybe "" (defaultLookupTextInDataTemplate dataTemplate "TicketId")


sdInvoiceLineDetailTemplate :: Double -> InvoiceLineDetailTemplate
sdInvoiceLineDetailTemplate unitPrice =
  InvoiceLineDetailTemplate
    { dataTemplateToItemRef  = sdDataTemplateToItemRef
    , dataTemplateToQuantity = sdDataTemplateToQuantity
    , unitPrice              = unitPrice
    }


sdDataTemplateToItemRef :: DataTemplate -> ItemRef
sdDataTemplateToItemRef _ =
  (reference itemValue) { referenceName = maybeItemName }
  where
    itemValue :: Text
    itemValue = ""

    maybeItemName :: Maybe Text
    maybeItemName = Just ""


-- Amount: Produced Water : 150

sdDataTemplateToQuantity :: DataTemplate -> Maybe Double
sdDataTemplateToQuantity dataTemplate =
  case defaultLookupInDataTemplate dataTemplate "Amount" of
    Just (InputTypeDouble quantity) ->
      Just (_getInputDouble quantity)

    Just (InputTypeInt quantity) ->
      Just (fromIntegral (_getInputInt quantity))

    _ ->
      Nothing
