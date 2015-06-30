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
--
--------------------------------------------------------------------------------

-- | A QuickBooks invoice report context.

data InvoiceContext = InvoiceContext
  { invoiceReportContextTime :: UTCTime
  , invoiceTemplate          :: InvoiceTemplate
  , invoiceLineTemplate      :: InvoiceLineTemplate
  }


data InvoiceTemplate = InvoiceTemplate
  { dataTemplateToInvoiceCustomField1 :: Maybe (Form -> CustomField)
  , dataTemplateToInvoiceCustomField2 :: Maybe (Form -> CustomField)
  , dataTemplateToInvoiceCustomField3 :: Maybe (Form -> CustomField)
  }


defaultInvoiceTemplate :: InvoiceTemplate
defaultInvoiceTemplate =
  InvoiceTemplate
    { dataTemplateToInvoiceCustomField1 = Nothing
    , dataTemplateToInvoiceCustomField2 = Nothing
    , dataTemplateToInvoiceCustomField3 = Nothing
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
  { dataTemplateToQuantity :: DataTemplate -> Maybe Double
  }


defaultInvoiceLineDetailTemplate :: InvoiceLineDetailTemplate
defaultInvoiceLineDetailTemplate =
  InvoiceLineDetailTemplate
    { dataTemplateToQuantity = defaultDataTemplateToQuantity
    }


--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

defaultDataTemplateToQuantity
  :: DataTemplate
  -> Maybe Double

defaultDataTemplateToQuantity dataTemplate =
  case defaultDataTemplateToInputType dataTemplate "Amount" of
    Just (InputTypeDouble quantity) ->
      Just (_getInputDouble quantity)

    Just (InputTypeInt quantity) ->
      Just (fromIntegral (_getInputInt quantity))

    _ ->
      Nothing


--------------------------------------------------------------------------------
-- *
--------------------------------------------------------------------------------

-- **

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


-- **

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
      (reference customerValue) { referenceName = customerName }
      where
        customerName :: Maybe Text
        customerName = Nothing

        customerValue :: Text
        customerValue = ""

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


-- **

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
          [dataTemplateToLineCustomField1 <*> Just dataTemplate
          ,dataTemplateToLineCustomField2 <*> Just dataTemplate
          ,dataTemplateToLineCustomField3 <*> Just dataTemplate
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
    { salesItemLineDetailClassRef        = undefined -- :: !(Maybe ClassRef)
    , salesItemLineDetailUnitPrice       = undefined -- :: Maybe Double
    , salesItemLineDetailRatePercent     = undefined -- :: !(Maybe Double)
    , salesItemLineDetailPriceLevelRef   = undefined -- :: !(Maybe PriceLevelRef)
    , salesItemLineDetailQty             = maybeQuantity
    , salesItemLineDetailTaxCodeRef      = undefined -- :: !(Maybe TaxCodeRef)
    , salesItemLineDetailServiceData     = undefined -- :: !(Maybe Text)
    , salesItemLineDetailTaxInclusiveAmt = undefined -- :: !(Maybe Double)
    }
  where
    itemRef :: ItemRef
    itemRef = undefined

    maybeQuantity :: Maybe Double
    maybeQuantity =
      dataTemplateToQuantity dataTemplate


--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

-- |

dataTemplateItems
  :: DataTemplate
  -> [(Text,InputType)]

dataTemplateItems DataTemplate{templateItems} =
  fmap (\TemplateItem{..} -> (label,templateValue)) templateItems


-- |

defaultDataTemplateToInputType
  :: DataTemplate
  -> Text
  -> Maybe InputType

defaultDataTemplateToInputType dataTemplate =
  flip lookup (dataTemplateItems dataTemplate)
