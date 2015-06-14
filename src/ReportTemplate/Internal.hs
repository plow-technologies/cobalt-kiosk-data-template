{-# LANGUAGE RankNTypes #-}

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

module ReportTemplate.Internal (ReportTemplate
                                  , ReportPreambleLabel
                                  , ReportRowLabel
                               ,    ReportPreambleTemplate
                               ,    ReportRowTemplate
                                  , buildReportTemplate
                                  , buildReportPreambleTemplate
                                  , buildReportRowTemplate
                                  , reportPreambleTemplate
                                  , reportRowsTemplate
                               ,    reportRowLabels
                               ,    rowTransformMap
                               ,    ReportPreamble(..)
                               ,    ReportTable(..)
                               ,    ReportTableRowStyle(..)
                               ,    ReportTableColStyle(..)
                               ,    Report(..)
                               ,    RowNumber
                               ,    TableRowIndex
                               ,    TableColIndex
                               ,    renderReport
                                  , reportPreambleLabel
                                  , preambleTransformMap
                                  , foldrTableByRowWithIndex ) where


import           Control.Applicative ((<$>))
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M

-- Very simple labeling

type ReportPreambleLabel = String
type ReportRowLabel  = String


{--|
# A Description of the typeParameters

* 'context' --> Might be time or some positional value fields that help determine the proper coding
            whatever it is you only get to choose one thing for a given 'ReportTemplate'
* 'preambleSource' --> Whatever datatype you intend to use to build the  beginning portion of the report
                       Which is so often different than the rest of the report

* 'preambleSink' --> Often just a Text value or a JSON one, this is what you end up with after a preambleSource transformation

* 'rowSource' --> The datatype to build each row of the report
* 'rowSink'   --> the result of transforming the rowSource

|--}

-- | This is the main datatype that you build to generate the Rendered Report datatype
data ReportTemplate context preambleSource preambleSink rowSource rowSink = ReportTemplate {
     reportPreambleTemplate :: ReportPreambleTemplate context preambleSource preambleSink
   , reportRowsTemplate     :: ReportRowTemplate context rowSource rowSink }


buildReportTemplate
  :: [(ReportPreambleLabel,
       context -> preambleSource -> preambleSink)]
     -> [(ReportRowLabel, context -> rowSource -> rowSink)]
     -> ReportTemplate
          context preambleSource preambleSink rowSource rowSink
buildReportTemplate preambleTransformList rowTransformList = ReportTemplate
                                                                   (buildReportPreambleTemplate preambleTransformList)
                                                                   (buildReportRowTemplate rowTransformList)


-- | the Preamble Template consists of two parts
-- the '[ReportPreambleLabel]' which sets the order and content of the given data
-- the preambleTransformMap which takes a source and context for a given label and returns a sink
-- Notice that while each of the given field accessors is exported, the  'ReportPreambleTemplate' is
-- not.  The smart constructor must be used to build this.
data ReportPreambleTemplate context preambleSource preambleSink = ReportPreambleTemplate {
     reportPreambleLabel  :: [ReportPreambleLabel]
   , preambleTransformMap :: ReportPreambleRetrievalMap context preambleSource preambleSink
}



buildReportPreambleTemplate :: [( ReportPreambleLabel, context -> rowSource -> rowSink )] ->
                                     ReportPreambleTemplate context rowSource rowSink
buildReportPreambleTemplate labelXformList = ReportPreambleTemplate labelList labelMap
         where
           labelList = fst <$> labelXformList
           labelMap  = M.fromList labelXformList

type ReportPreambleRetrievalMap context preambleSource preambleSink =
                       Map ReportPreambleLabel (context -> preambleSource -> preambleSink )


data ReportRowTemplate context  rowSource rowSink = ReportRowTemplate {
      reportRowLabels :: [ReportRowLabel]
    , rowTransformMap :: ReportRowRetrievalMap context rowSource rowSink
}

buildReportRowTemplate :: [( ReportRowLabel, context -> rowSource -> rowSink )] ->
                             ReportRowTemplate context rowSource rowSink
buildReportRowTemplate labelXformList = ReportRowTemplate labelList labelMap
         where
           labelList = fst <$> labelXformList
           labelMap  = M.fromList labelXformList

type ReportRowRetrievalMap context rowSource rowSink =
                             Map ReportRowLabel (context -> rowSource -> rowSink)


-- | The order that things happen is often very important so an Integer
-- is used as the start ord value for both things



data Report preambleValue rowValue = Report {
          _reportPreamble :: ReportPreamble preambleValue
        , _reportRows     :: ReportTable rowValue }

data ReportPreamble preambleValue = ReportPreamble {
        _preambleValue :: [(ReportPreambleLabel, preambleValue)]
  }



-- Map sorts first by row and then by

type RowNumber = Int
type TableRowIndex = (RowNumber, ReportRowLabel)


{-| even though the whole report here is designed around row data
    There are many times we need col based data, totals being an obvious one
    So you can select which way you want the data sorted and switch between them with
    the transposeReportTableFunction which changes to the other of whichever version you are using
|-}

data ReportTable rowValue =  ReportTableRowIndex   ( [ReportRowLabel], ReportTableRowStyle rowValue)
                            | ReportTableColIndex    ([ReportRowLabel], ReportTableColStyle rowValue)


data ReportTableRowStyle rowValue = ReportTableRowStyle {
    _rowMap ::  Map TableRowIndex rowValue
 }


type TableColIndex = (ReportRowLabel,RowNumber)

data ReportTableColStyle rowValue = ReportTableColStyle {
    _colMap ::  Map TableRowIndex rowValue
 }


renderReport :: ReportTemplate context preIn preOut rowIn rowOut ->
                  context  -> preIn -> [rowIn] ->  Report preOut rowOut
renderReport reportTemplate context preIn rows = Report reportPreambleOut rowOut
  where
    reportPreambleOut = ReportPreamble . transformPrein context preIn preambleLabels $ preambleOutTransformMap
    rowOut = ReportTableRowIndex (rowTransformLabels , ReportTableRowStyle . transformRows context rowTransformLabels rowOutTransformMap $ rows)
    preambleOutTransformMap = preambleTransformMap.reportPreambleTemplate $ reportTemplate
    preambleLabels = reportPreambleLabel.reportPreambleTemplate $ reportTemplate
    rowOutTransformMap = rowTransformMap . reportRowsTemplate $ reportTemplate
    rowTransformLabels = reportRowLabels . reportRowsTemplate $ reportTemplate


-- |Transform Preamble
-- transformPrein :: ReportPreambleRetrievalMap context preamble

transformPrein :: context -> preambleSource -> [ReportPreambleLabel] ->
                  Map ReportPreambleLabel (context -> preambleSource -> preambleSink) ->
                  [(ReportPreambleLabel, preambleSink)]
transformPrein context preIn preambleTransformLabels mp = foldr (\l lst -> case M.lookup l mp of
                                                                                  Nothing -> lst
                                                                                  (Just f) -> (l, f context preIn):lst ) [] preambleTransformLabels


-- |Transform rows
transformRows :: forall rowSource rowOut context.
                       context
                       -> [ReportRowLabel]
                       -> ReportRowRetrievalMap context rowSource rowOut
                       -> [rowSource]
                       -> Map TableRowIndex rowOut
transformRows context rowTransformLabels rowOutTransformMap rows = M.unions $ zipWith (transformRowin context rowTransformLabels rowOutTransformMap  ) [1 ..] rows

transformRowin :: forall rowOut  context rowSource.
                        context -> [ReportRowLabel] -> ReportRowRetrievalMap context rowSource rowOut -> RowNumber -> rowSource -> Map TableRowIndex rowOut
transformRowin context rowTransformLabels mp i row  = foldr (\l mpTbl -> case M.lookup l mp  of
                                                                                  Nothing -> mpTbl
                                                                                  (Just f) -> M.insert (i,l) (f context row) mpTbl ) M.empty  rowTransformLabels



-- Traversing functions
-- because we know the form of this map is tabular
-- the fold can be made a bit more useful and track
-- both the
foldrTableByRowWithIndex :: (TableRowIndex ->
                            a ->
                            b -> b)
                            -> b -> ReportTableRowStyle a ->  b
foldrTableByRowWithIndex f b rowTable = M.foldrWithKey f b $
                                        _rowMap rowTable


