{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Kiosk.Backend.Table (TemplateTable(..)
                           ,getTemplateTable) where

import           Data.Table                      (Key, PKT, Primary,
                                                  Supplemental, Tab, Table,
                                                  Tabular, fetch, forTab,
                                                  fromList, ixTab, mkTab,
                                                  primarily, primary)
import           Control.Lens                    (makeLenses, view)                                                  
import           Data.Serialize                  (Serialize, get, put)
import Kiosk.Backend.Data.DataTemplateEntry 
import Kiosk.Backend.Data.DataTemplateEntryKey
import           Data.Aeson.Serialize            (getFromJSON, putToJSON)
import Data.Aeson (ToJSON
                  ,FromJSON
                  ,parseJSON
                  ,toJSON
                  ,fromJSON)
import           Data.Foldable                   (toList) 
import Control.Applicative ((<$>)
                           ,(<*>))

-- | Tabular Instances
newtype TemplateTable = TemplateTable {_getTemplateTable ::  Table DataTemplateEntry}
                    deriving (Show,Eq)
makeLenses ''TemplateTable



instance ToJSON TemplateTable where
      toJSON = toJSON.toList._getTemplateTable

instance FromJSON TemplateTable where
      parseJSON v = TemplateTable . fromList <$>
                        parseJSON v


instance Serialize TemplateTable where
         put = putToJSON
         get = getFromJSON

