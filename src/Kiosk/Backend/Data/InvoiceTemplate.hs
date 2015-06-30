{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Data.InvoiceTemplate where

import Kiosk.Backend.Data
import Kiosk.Backend.Form
import QuickBooks
import ReportTemplate.Internal

import Data.Text (Text)
import Data.Time (UTCTime)

-- | A QuickBooks report.

type QuickBooksReport =
  Report Invoice Line


-- |

data QuickBooksReportContext = QuickBooksReportContext
  { quickBooksReportContextCurrentTime :: UTCTime
  }


-- | A QuickBooks report template.

type QuickBooksReportTemplate =
  ReportTemplate QuickBooksReportContext Form Invoice DataTemplate Line


-- | Generate a QuickBooks invoice given a report.

renderQuickBooksReport
  :: QuickBooksReport   -- ^
  -> Invoice            -- ^

renderQuickBooksReport quickBooksReport =
  defaultInvoice lines customerRef
  where
    customerRef :: CustomerRef
    customerRef = undefined
--      (reference referenceValue) {referenceName = Just referenceName}

    referenceValue :: Text
    referenceValue = ""

    referenceName :: Text
    referenceName = ""

    lines :: [Line]
    lines = quickBooksReportToLines quickBooksReport


-- | Generate QuickBooks lines given a report.

quickBooksReportToLines
  :: QuickBooksReport
  -> [Line]

quickBooksReportToLines quickBooksReport =
  fmap dataTemplateToLine dataTemplates
  where
    dataTemplates :: [DataTemplate]
    dataTemplates = undefined


-- |

dataTemplateToLine
  :: DataTemplate
  -> Line

dataTemplateToLine dataTemplate =
  salesItemLine amount detail
  where
    amount :: Double
    amount = undefined

    detail :: SalesItemLineDetail
    detail = salesItemLineDetail itemRef

    itemRef :: Reference
    itemRef = undefined

-- |

dataTemplateToLines
  :: DataTemplate
  -> [Line]

dataTemplateToLines DataTemplate{templateItems} =
  fmap templateItemToLine templateItems


-- |

templateItemToLine
  :: TemplateItem
  -> Line

templateItemToLine (TemplateItem label (InputTypeDouble sd)) =
  (salesItemLine amount detail)
    { lineDescription = Just label }
  where
    amount :: Double
    amount = _getInputDouble sd

    detail :: SalesItemLineDetail
    detail = salesItemLineDetail itemRef

    itemRef :: ItemRef
    itemRef = reference "21"

templateItemToLine _ = undefined
