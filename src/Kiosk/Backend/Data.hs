{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# Language RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kiosk.Backend.Data ( DataTemplateEntry (..)
                           ,DataTemplateEntryKey (..)
                          , TemplateTable (..)
                          , dataTemplateEntryKey
                          , dataTemplateEntryValue
                          , getTemplateTable
                          , TemplateTable) where

-- Types
import           Data.Aeson                      (FromJSON, ToJSON,
                                                  Value (Object), object,
                                                  parseJSON, toJSON, (.:), (.=))
import           Data.UUID                       (UUID, fromString, toString)
-- Control
import           Control.Applicative             ((<$>), (<*>))
import           Data.Aeson.Types                (Parser ()
                                                 ,Value(..))
import Data.Aeson.Serialize (putToJSON
                            ,getFromJSON)
import Data.Serialize (Serialize
                      ,get
                      ,put)
import           Kiosk.Backend.Data.DataTemplate (DataTemplate)
import Data.Foldable (toList)
import Control.Lens (makeLenses
                    ,(^.)
                    ,views
                    )
import Data.Table  (Table
                   ,Tabular
                   ,PKT
                   ,Key
                   ,Tab
                   ,Primary
                   ,Supplemental
                   ,fromList
                   ,table
                   ,fetch
                   ,primary
                   ,primarily
                   ,mkTab
                   ,ixTab
                   ,forTab)


-- |Key for Data Template
   
data DataTemplateEntryKey = DataTemplateEntryKey {
                          _getDate   :: Int , 
                          _getUUID   :: UUID,
                          _getFormId :: Int
                          }
   deriving (Eq,Ord,Show)

instance ToJSON DataTemplateEntryKey where
  toJSON (DataTemplateEntryKey fId uuid date) = object [
                                                "date" .= date
                                              , "uuid" .= toString uuid
                                              , "formid" .= fId]

instance FromJSON DataTemplateEntryKey where
  parseJSON (Object o) = DataTemplateEntryKey <$> o .: "date"
                                              <*> ((o .: "uuid") >>= decodeUUID)
                                              <*> o .: "formid" 
  parseJSON _ = fail "Expecting DataTemplateEntryKey Object, Received Other"

decodeUUID :: Value -> Parser UUID
decodeUUID v = do
           uuid <- fromString <$> parseJSON v
           case uuid of
                Just decodeId -> return decodeId
                Nothing -> fail "Unable to parse UUID, please check String format."



-- |Data Template Entry defines a return value of a form
data DataTemplateEntry = DataTemplateEntry {
                       _dataTemplateEntryKey :: DataTemplateEntryKey,
                       _dataTemplateEntryValue    :: DataTemplate
                                                 }
           deriving (Show,Eq)

makeLenses ''DataTemplateEntry
-- | Aeson Instances                                           
instance ToJSON DataTemplateEntry where
  toJSON (DataTemplateEntry k v) = object ["key" .= k
                                          ,"value" .= v]

instance FromJSON DataTemplateEntry where
  parseJSON (Object o) = DataTemplateEntry <$> o .: "key"
                                           <*> o .: "value"
  parseJSON _          = fail "Expecting DateTemplateEntry object, Received Other"



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
