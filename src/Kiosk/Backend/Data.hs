{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Data ( DataTemplateEntry (..)
                           ,DataTemplateEntryKey (..)) where

-- Types
import           Data.Aeson                      (FromJSON, ToJSON,
                                                  Value (Object), object,
                                                  parseJSON, toJSON, (.:), (.=))
import           Data.UUID                       (UUID, fromString, toString)
-- Control
import           Control.Applicative             ((<$>), (<*>))
import           Data.Aeson.Types                (Parser ())
import           Kiosk.Backend.Data.DataTemplate (DataTemplate)

data DataTemplateEntry = DataTemplateEntry {
                       dataTemplateEntryKey :: DataTemplateEntryKey,
                       dataTemplateValue    :: DataTemplate
                                           }
instance ToJSON DataTemplateEntry where
  toJSON (DataTemplateEntry k v) = object ["key" .= k
                                          ,"value" .= v]

instance FromJSON DataTemplateEntry where
  parseJSON (Object o) = DataTemplateEntry <$> o .: "key"
                                           <*> o .: "value"
  parseJSON _          = fail "Expecting DateTemplateEntry object, Received Other"

data DataTemplateEntryKey = DataTemplateEntryKey {
                          getFormId :: Int,
                          getUUID   :: UUID,
                          getDate   :: Int }

instance ToJSON DataTemplateEntryKey where
  toJSON (DataTemplateEntryKey fId uuid date) = object [
                                                "formid" .= fId
                                              , "uuid" .= toString uuid
                                              , "date" .= date]

instance FromJSON DataTemplateEntryKey where
  parseJSON (Object o) = DataTemplateEntryKey <$> o .: "formid"
                                              <*> ((o .: "uuid") >>= decodeUUID)
                                              <*> o .: "date"
  parseJSON _ = fail "Expecting DataTemplateEntryKey Object, Received Other"

decodeUUID :: Value -> Parser UUID
decodeUUID v = do
           uuid <- fromString <$> parseJSON v
           case uuid of
                Just decodeId -> return decodeId
                Nothing -> fail "Unable to parse UUID, please check String format."
