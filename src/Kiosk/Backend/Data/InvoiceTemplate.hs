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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)


--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

-- | A QuickBooks invoice report.

type InvoiceReport =
  Report Invoice Line


-- | A QuickBooks invoice report context.

data InvoiceReportContext = QuickBooksReportContext
  { invoiceReportContextTime :: UTCTime
  }


-- | A QuickBooks invoice report template.

type InvoiceReportTemplate =
  ReportTemplate InvoiceReportContext Form Invoice DataTemplate Line


-- |

buildInvoiceReport
  :: InvoiceReportTemplate
  -> InvoiceReportContext
  -> Form
  -> [DataTemplate]
  -> InvoiceReport

buildInvoiceReport  =
  renderReport


-- |

renderInvoice
  :: InvoiceReport
  -> Invoice

renderInvoice =
  undefined


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
--
--------------------------------------------------------------------------------

-- |

formAndDataTemplatesToInvoice
  :: Form
  -> [DataTemplate]
  -> Invoice

formAndDataTemplatesToInvoice form dataTemplates =
  invoice { invoiceLine = invoiceLines }
  where
    invoice :: Invoice
    invoice =
      formToInvoice form

    invoiceLines =
      dataTemplatesToLines dataTemplates


-- |

formToInvoice
  :: Form
  -> Invoice

formToInvoice _ =
  (defaultInvoice [] customerRef)
    { invoiceCustomField = Nothing -- Maybe [CustomField]
    }
  where
    customerRef :: CustomerRef
    customerRef =
      undefined


-- |

dataTemplatesToLines
  :: [DataTemplate]
  -> [Line]

dataTemplatesToLines =
  fmap dataTemplateToLine


-- |

dataTemplateToLine
  :: DataTemplate
  -> Line

dataTemplateToLine =
  dataTemplateToSalesItemLine


-- |

dataTemplateToSalesItemLine
  :: DataTemplate
  -> Line

dataTemplateToSalesItemLine dataTemplate =
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
      undefined
      -- dataTemplateToSalesItemLineDetail invoiceLineDetailTemplate dataTemplate

    maybeCustomFields :: Maybe [CustomField]
    maybeCustomFields =
      undefined
      -- dataTemplateToCustomFields <*> Just dataTemplate

    maybeDescription :: Maybe Text
    maybeDescription =
      undefined
      -- dataTemplateToLineDescription <*> Just dataTemplate


-- |

dataTemplateToSalesItemLineDetail
  :: DataTemplate
  -> SalesItemLineDetail

dataTemplateToSalesItemLineDetail dataTemplate =
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
      undefined
      -- dataTemplateToQuantity dataTemplate


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
