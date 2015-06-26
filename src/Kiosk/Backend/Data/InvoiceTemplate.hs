module Kiosk.Backend.Data.InvoiceTemplate
  ( exportToQuickBooksInvoice
  )
  where

import Kiosk.Backend.Data.DataTemplate
import QuickBooks

exportToQuickBooksInvoice :: [DataTemplate] -> Invoice
exportToQuickBooksInvoice _ =
  undefined
