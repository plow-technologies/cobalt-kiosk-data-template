{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Kiosk.Backend.Table (TemplateTable(..)
                           ,getTemplateTable) where

import           Data.Table                      (
                                                  Table,
                                                    
                                                  fromList,  
                                                  )
import           Control.Lens                    (makeLenses)                  
import           Data.Serialize                  (Serialize, get, put)
import Kiosk.Backend.Data.DataTemplateEntry 

import           Data.Aeson.Serialize            (getFromJSON, putToJSON)
import Data.Aeson (ToJSON
                  ,FromJSON
                  ,parseJSON
                  ,toJSON)
import           Data.Foldable                   (toList) 
import Control.Applicative ((<$>))

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

