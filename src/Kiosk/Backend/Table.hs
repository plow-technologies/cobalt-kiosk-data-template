{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Kiosk.Backend.Table () where

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
import           Kiosk.Backend.Data.DataTemplate         (DataTemplate (..))
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

instance Tabular DataTemplateEntry where
      type PKT DataTemplateEntry  = DataTemplateEntryKey
      data Key k DataTemplateEntry b where
        Key :: Key Primary DataTemplateEntry DataTemplateEntryKey
        DValue :: Key Supplemental DataTemplateEntry DataTemplate
      data Tab DataTemplateEntry i = DTab (i Primary DataTemplateEntryKey) (i Supplemental DataTemplate)
      fetch Key = _dataTemplateEntryKey
      fetch DValue = _dataTemplateEntryValue
      primary = Key
      primarily Key r = r
      mkTab f = DTab <$> f Key <*> f DValue
      forTab (DTab i s) f = DTab <$> f Key i <*> f DValue s
      ixTab (DTab i _ ) Key = i
      ixTab (DTab _ vs) DValue = vs
