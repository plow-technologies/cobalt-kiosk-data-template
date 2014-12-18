{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Kiosk.Backend.Table () where

import qualified Data.Table                      as T
import           Kiosk.Backend.Data.DataTemplate

type TemplateTable = T.Table DataTemplate

-- insertDataTemplate :: DataTemplate -> TemplateTable -> TemplateTable
-- insertDataTemplate dt t = T.insert dt t
