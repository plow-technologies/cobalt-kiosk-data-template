
{- |
Module      :  ReportTemplate.Internal
Description :  Generate Row based reports with headers and footers
Copyright   :  Plow Technologies LLC
License     :  MIT License
Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable


There are lots of times when the situation is 2 DataTypes are to be combined into a document.

One representing the fixed descriptive of a document and the other representing a running set of
data rows.

This is a generic library for transforming data into this form.

-}
module ReportTemplate.Internal () where

import           Data.Map.Strict

type ReportHeaderLabel = String
type ReportRowLabel  = String

type ReportHeaderRetrievalMap context headerSource headerSink =
                             Map ReportHeaderLabel (context -> headerSource -> headerSink )

type ReportRowRetrievalMap context rowSource rowSink =
                             Map ReportRowLabel (context -> rowSource -> rowSink)

data ReportTemplate context headerSource headerSink rowSource rowSink = ReportTemplate {
       reportHeader :: [ReportHeaderTemplate context headerSource headerSink]
     , reportRows   :: [ReportRowTemplate context rowSource rowSink]}

data ReportHeaderTemplate context headerSource headerSink = ReportHeaderTemplate {
     reportHeaderLabel       :: [ReportHeaderLabel ]
   , valueRetrievalFunctions :: [ReportHeaderRetrievalMap context headerSource headerSink ]
}



data ReportRowTemplate context  rowSource rowSink = ReportRowTemplate {
      reportRowLabels             :: [ReportRowLabel]
    , reportRowRetrievalFunctions :: ReportRowRetrievalMap context rowSource rowSink
}



