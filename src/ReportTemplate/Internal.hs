
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

The general form is:


  <report preface label 1> <label 1 value></report preface label 1>
  <report preface label 2> <label 2 value></report preface label 2>
  <report preface label 3> <label 3 value></report preface label 3>
  <report preface label 4> <label 4 value></report preface label 4>  

<column label 1> <column label 2> <column label 3> ... <column label n>
<r1c1 data>      <r1c2 data>      <r1c3 data>      ... <r1cn data>       
.                                                   .
.                                                   .
.                                                   .                                                  
<rnc1 data>     <rnc2 data>       <rnc3 data>      ... <rncn data>

-}
module ReportTemplate.Internal () where

import           Data.Map.Strict

type ReportPrefaceLabel = String
type ReportRowLabel  = String

type ReportRetrievalMap context prefaceSource prefaceSink =
                       Map ReportPrefaceLabel (context -> prefaceSource -> prefaceSink )

type ReportRowRetrievalMap context rowSource rowSink =
                             Map ReportRowLabel (context -> rowSource -> rowSink)

data ReportTemplate context prefaceSource prefaceSink rowSource rowSink = ReportTemplate {
     reportPreface :: [ReportPrefaceTemplate context prefaceSource prefaceSink]
     , reportRows   :: [ReportRowTemplate context rowSource rowSink]}

data ReportPrefaceTemplate context prefaceSource prefaceSink = ReportPrefaceTemplate {
     reportPrefaceLabel       :: [ReportPrefaceLabel ]
   , valueRetrievalFunctions :: [ReportPrefaceRetrievalMap context prefaceSource prefaceSink ]
}

data ReportRowTemplate context  rowSource rowSink = ReportRowTemplate {
      reportRowLabels             :: [ReportRowLabel]
    , reportRowRetrievalFunctions :: ReportRowRetrievalMap context rowSource rowSink
}




data RenderedReport prefaceValue rowValue = RenderedReport {
         reportPreface :: }
