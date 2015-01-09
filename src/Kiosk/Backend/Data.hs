{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Kiosk.Backend.Data ( DataTemplateEntry (..)
                           ,DataTemplateEntryKey (..)
                          , DataTemplate (..)
                          , TemplateTable (..)
                          , TicketId (..)
                          , dataTemplateEntryKey
                          , dataTemplateEntryValue
                          , getTemplateTable
                          , decodeUUID
                          , getListOfSortedTemplateItems
                          , fromDataTemplateEntryToCsv
                          , fromDataTemplateEntryToS3Csv) where

-- Types
import           Data.Aeson                      (FromJSON, ToJSON,
                                                  Value (Object), object,
                                                  parseJSON, toJSON, (.:), (.=))
import           Data.UUID                       (UUID, fromString, toString)
-- Control
import           Control.Applicative             ((<$>), (<*>))
import           Control.Lens                    (makeLenses, view)
import           Control.Monad                   (liftM)
import           Data.Aeson.Serialize            (getFromJSON, putToJSON)
import           Data.Aeson.Types                (Parser (), Value (..))
import qualified Data.ByteString.Lazy            as LBS (append, concat,
                                                         fromStrict, length,
                                                         take)
import           Data.ByteString.Lazy.Internal   (ByteString)
import qualified Data.Csv                        as C (ToRecord, encode,
                                                       toField, toRecord)
import           Data.Foldable                   (toList)
import qualified Data.List                       as L (sort)
import           Data.Serialize                  (Serialize, get, put)
import           Data.Table                      (Key, PKT, Primary,
                                                  Supplemental, Tab, Table,
                                                  Tabular, fetch, forTab,
                                                  fromList, ixTab, mkTab,
                                                  primarily, primary)
import qualified Data.Text                       as T (Text, breakOn, drop,
                                                       unpack)
import qualified Data.Vector                     as V (fromList, (++))
import           Kiosk.Backend.Data.DataTemplate (DataTemplate (..),
                                                  InputText (..),
                                                  InputType (..),
                                                  TemplateItem (..),
                                                  fromDataTemplateToCSV,
                                                  _templateItems)


-- |Key for Data Template

newtype TicketId = TicketId {_getTicketIdPair :: (Int,Int) } deriving (Eq, Ord, Show)

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
                                    where lst = [C.toField d, C.toField fid, C.toField . ticketIdToString $ tid, C.toField . toString $ uid ]

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


ticketIdToString :: TicketId -> String
ticketIdToString (TicketId (a,b)) = show a ++ "_" ++ show b

decodeTicketID :: Value -> Parser TicketId
decodeTicketID (String s) = do
         let (s1, s2) = splitString s
         return $ TicketId (read s1, read s2)
decodeTicketID _ = fail "Expected String, Received Other"

splitString :: T.Text -> (String, String)
splitString s = (T.unpack t1, T.unpack $ T.drop 1 t2)
      where (t1, t2) = T.breakOn ("-"::T.Text) s


decodeUUID :: Value -> Parser UUID
decodeUUID v = do
           uuid <- fromString <$> parseJSON v
           case uuid of
                Just decodeId -> return decodeId
                Nothing -> fail "Unable to parse UUID, please check String format."


-- |Data Template Entry defines a return value of a form
data DataTemplateEntry = DataTemplateEntry {
                       _dataTemplateEntryKey   :: DataTemplateEntryKey,
                       _dataTemplateEntryValue :: DataTemplate
                                                 }
           deriving (Show,Eq,Ord)


instance C.ToRecord DataTemplateEntry where
  toRecord (DataTemplateEntry dtk dtv) = C.toRecord dtk V.++ C.toRecord dtv

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

fromDataTemplatesEntryToDataTemplates :: [DataTemplateEntry] -> [DataTemplate]
fromDataTemplatesEntryToDataTemplates dtes = view dataTemplateEntryValue <$> dtes

getListOfSortedTemplateItems :: DataTemplate -> [TemplateItem]
getListOfSortedTemplateItems dts = L.sort $ templateItems dts

fromLabelsToHeaders :: [TemplateItem] -> [ByteString]
fromLabelsToHeaders tis = flip LBS.append "," <$> (LBS.fromStrict . C.toField . label <$> tis)

fromDataTemplateEntryToS3Csv :: [DataTemplateEntry] -> ByteString
fromDataTemplateEntryToS3Csv templateEntries = LBS.append (getHeaders templatesWithSortedItems) (fromDataTemplateToCSV templatesWithSortedItems)
                           where dataTemplates  = fromDataTemplatesEntryToDataTemplates templateEntries
                                 templatesWithSortedItems = sortDataTemplates <$> dataTemplates

getHeaders :: [DataTemplate] -> ByteString
getHeaders [] = ""
getHeaders lstOfTemplates = LBS.append dropComma "\r\n"
               where  bs = LBS.concat . fromLabelsToHeaders . templateItems . head $ lstOfTemplates
                      dropComma = LBS.take (LBS.length bs -1) bs

appendKeyHeaders :: ByteString -> ByteString
appendKeyHeaders = LBS.append defaultKeyHeaders

defaultKeyHeaders :: ByteString
defaultKeyHeaders = "Date,FormId,TicketId,UUID,"

sortDataTemplates :: DataTemplate -> DataTemplate
sortDataTemplates dts = dts {templateItems = newDts}
             where newDts = L.sort . filterTemplateItems $ view _templateItems dts

sortDataTemplatesWRemoveField :: DataTemplate -> DataTemplate
sortDataTemplatesWRemoveField dts = dts {templateItems = newDts}
             where newDts = L.sort . filterTemplateItems $ view _templateItems dts

sortDataTemplatesEntries :: [DataTemplateEntry] -> [DataTemplateEntry]
sortDataTemplatesEntries dtes = sortDataTemplatesEntry <$> dtes

sortDataTemplatesEntry :: DataTemplateEntry -> DataTemplateEntry
sortDataTemplatesEntry dte = dte {_dataTemplateEntryValue =s}
       where s = sortDataTemplatesWRemoveField $ view dataTemplateEntryValue dte

filterTemplateItems :: [TemplateItem] -> [TemplateItem]
filterTemplateItems = filter notSignature

notSignature :: TemplateItem -> Bool
notSignature (TemplateItem ("signature"::T.Text) (InputTypeText (InputText _))) = False
notSignature _ = True

fromDataTemplateEntryToCsv :: [DataTemplateEntry] -> ByteString
fromDataTemplateEntryToCsv templateEntries = LBS.append (appendKeyHeaders . getHeaders $ templatesWithSortedItems) (C.encode . sortDataTemplatesEntries $ templateEntries)
                           where dataTemplates  = fromDataTemplatesEntryToDataTemplates templateEntries
                                 templatesWithSortedItems = sortDataTemplatesWRemoveField <$> dataTemplates
