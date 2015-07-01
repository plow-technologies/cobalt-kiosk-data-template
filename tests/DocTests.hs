module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main =
  doctest ["-isrc", "src/Kiosk/Backend/Data/InvoiceTemplate.hs"]
