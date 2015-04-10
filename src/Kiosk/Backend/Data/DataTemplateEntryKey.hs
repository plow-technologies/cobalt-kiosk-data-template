{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Kiosk.Backend.Data.DataTemplateEntryKey
Description :  Key defining a data template entry uniquely
Copyright   :  Plow Technologies LLC
License     :  MIT License
Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

-}

module Kiosk.Backend.Data.DataTemplateEntryKey (DataTemplateEntryKey (..)
                                               ,TicketId(..)
                                               ,decodeUUID) where

import           Data.Time           (formatTime, utcToZonedTime)

import           Plow.Extras.Time    (intToUTCTime)

import           System.Locale       (defaultTimeLocale)

import           Data.Time.LocalTime (TimeZone (..))

import           Control.Applicative ((<$>), (<*>))

import           Control.Monad       (liftM)

import           Data.Aeson          (FromJSON, ToJSON, Value (..), object,
                                      parseJSON, toJSON, (.:), (.=))

import           Data.Aeson.Types    (Parser ())

import qualified Data.Csv            as C

import qualified Data.Text           as T

import           Data.UUID           (UUID, fromString, toString)

import qualified Data.Vector         as V

-- |Key for Data Template
newtype TicketId = TicketId {_getTicketIdPair :: (Int,Int) } deriving (Eq, Ord, Show)

decodeTicketID :: Value -> Parser TicketId
decodeTicketID (String s) = do
         let (s1, s2) = splitString s
         return $ TicketId (read s1, read s2)
decodeTicketID _ = fail "Expected String, Received Other"

ticketIdToString :: TicketId -> String
ticketIdToString (TicketId (a,b)) = show a ++ "_" ++ show b

intTimeToHumanTime :: Int -> String
intTimeToHumanTime intTime = formatTime defaultTimeLocale "%Y/%m/%dT%H:%M:%S" time
                      where utcTime = intToUTCTime . div intTime $ 1000
                            time = utcToZonedTime oklahomaTimeZone utcTime

oklahomaTimeZone :: TimeZone
oklahomaTimeZone = TimeZone (-360) False "CST"

splitString :: T.Text -> (String, String)
splitString s = (T.unpack t1, T.unpack $ T.drop 1 t2)
      where (t1, t2) = T.breakOn ("-"::T.Text) s

decodeUUID :: Value -> Parser UUID
decodeUUID v = do
           uuid <- fromString <$> parseJSON v
           case uuid of
                Just decodeId -> return decodeId
                Nothing -> fail "Unable to parse UUID, please check String format."

instance ToJSON TicketId where
  toJSON (TicketId (a,b)) = toJSON (show a ++ "-" ++ show b)

data DataTemplateEntryKey = DataTemplateEntryKey {
                          _getDate     :: Int ,
                          _getUUID     :: UUID,
                          _getTicketId :: TicketId,
                          _getFormId   :: Int
                          }
   deriving (Eq,Ord,Show)


instance C.ToRecord DataTemplateEntryKey  where
  toRecord (DataTemplateEntryKey d uid tid fid ) = V.fromList $ C.toField <$> lst
                                    where lst = [C.toField (intTimeToHumanTime d)
                                                , C.toField fid
                                                , C.toField . ticketIdToString $ tid
                                                , C.toField . toString $ uid ]

instance ToJSON DataTemplateEntryKey where
  toJSON (DataTemplateEntryKey date uuid ticketid fId) = object [
                                                "date" .= show date
                                              , "uuid" .= toString uuid
                                              , "ticketid" .= ticketid
                                              , "formid" .= show fId ]

instance FromJSON DataTemplateEntryKey where
  parseJSON (Object o) = DataTemplateEntryKey <$> liftM read (o .: "date")
                                              <*> ((o .: "uuid") >>= decodeUUID)
                                              <*> ((o .: "ticketid") >>= decodeTicketID)
                                              <*> liftM read (o .: "formid")
  parseJSON _ = fail "Expecting DataTemplateEntryKey Object, Received Other"
