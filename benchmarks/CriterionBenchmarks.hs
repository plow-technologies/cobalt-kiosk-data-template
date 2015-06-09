{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Kiosk.Backend.Data
import           Kiosk.Backend.Form

import           Kiosk.Backend.Data.DataTemplateEntry (fromDataTemplateEntryToXlsxWorksheet)

import           Criterion.Main                       (bench, bgroup,
                                                       defaultMain, whnf)
import           Data.Maybe                           (fromJust)
import           Data.UUID                            (fromString)

fromDataTemplateEntryToXlsx  = fromDataTemplateEntryToXlsxWorksheet

main :: IO ()
main =
  defaultMain
    [bgroup
       "from data template entries to xlsx"
       [bench
          "300 entries"
          (whnf fromDataTemplateEntryToXlsx (benchDataTemplateEntries 300))
       ,bench
          "600 entries"
          (whnf fromDataTemplateEntryToXlsx (benchDataTemplateEntries 600))
       ,bench
          "900 entries"
          (whnf fromDataTemplateEntryToXlsx (benchDataTemplateEntries 900))]]

benchDataTemplateEntries :: Int -> [DataTemplateEntry]
benchDataTemplateEntries = flip replicate benchDataTemplateEntry

benchDataTemplateEntry :: DataTemplateEntry
benchDataTemplateEntry = DataTemplateEntry
  { _dataTemplateEntryKey   = benchDataTemplateEntryKey
  , _dataTemplateEntryValue = benchDataTemplateEntryValue
  }

benchDataTemplateEntryKey :: DataTemplateEntryKey
benchDataTemplateEntryKey = DataTemplateEntryKey
  { _getDate     = 1
  , _getUUID     = fromJust (fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c")
  , _getTicketId = TicketId (1,1)
  , _getFormId   = 1
  }

benchDataTemplateEntryValue :: DataTemplate
benchDataTemplateEntryValue = DataTemplate
  { templateItems =
      [TemplateItem "A" (InputTypeText (InputText "1"))
      ,TemplateItem "B" (InputTypeText (InputText "2"))
      ,TemplateItem "C" (InputTypeText (InputText "3"))
      ,TemplateItem "D" (InputTypeText (InputText "4"))
      ,TemplateItem "E" (InputTypeText (InputText "5"))
      ,TemplateItem "F" (InputTypeText (InputText "6"))
      ,TemplateItem "G" (InputTypeText (InputText "7"))
      ,TemplateItem "H" (InputTypeText (InputText "8"))
      ,TemplateItem "I" (InputTypeText (InputText "9"))
      ,TemplateItem "J" (InputTypeText (InputText "10"))
      ,TemplateItem "K" (InputTypeText (InputText "11"))
      ,TemplateItem "L" (InputTypeText (InputText "12"))
      ,TemplateItem "M" (InputTypeText (InputText "13"))
      ,TemplateItem "N" (InputTypeText (InputText "14"))
      ,TemplateItem "O" (InputTypeText (InputText "15"))
      ,TemplateItem "P" (InputTypeText (InputText "16"))
      ,TemplateItem "Q" (InputTypeText (InputText "17"))
      ,TemplateItem "R" (InputTypeText (InputText "18"))
      ,TemplateItem "S" (InputTypeText (InputText "19"))
      ,TemplateItem "T" (InputTypeText (InputText "20"))
      ,TemplateItem "U" (InputTypeText (InputText "21"))
      ,TemplateItem "V" (InputTypeText (InputText "22"))
      ,TemplateItem "W" (InputTypeText (InputText "23"))
      ,TemplateItem "X" (InputTypeText (InputText "24"))
      ,TemplateItem "Y" (InputTypeText (InputText "25"))
      ,TemplateItem "Z" (InputTypeText (InputText "26"))]
  }
