{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Kiosk.Backend.Data where

-- import Kiosk.Backend.Data.Internal
import Kiosk.Backend.Form (Item(..)
                          ,Form(..)
                          ,Company (..)
                          ,Address(..)
                          ,Input(..))
import Data.Aeson
import  Data.Aeson.Types  
import Data.Text (Text)
import Control.Applicative ((<$>), (<*>) )
import           Control.Monad             (mzero)
import qualified Data.HashMap.Strict as HM (toList)
import Control.Lens (makeLenses)

-- Data Template Type
data DataTemplate = DataTemplate { company::Company,
                                   address :: Address, 
                                   templateItems :: [TemplateItem]}
data TemplateItem = TemplateItem {
            label :: Text
            , templateValue :: Input }

-- Make Lenses

makeLenses ''Form
makeLenses ''DataTemplate            


-- JSON Instances
-- | encode a list of items as a flat single object instead of as an array of objects
encodeTemplateItemsAsObject :: [TemplateItem] -> Value
encodeTemplateItemsAsObject items = object $ fmap objectMaker items
                      where
                       objectMaker (TemplateItem { label=l,
                                            templateValue=v}) = l .= v
decodeInput :: Value -> Parser Input
decodeInput = parseJSON 


decodeObjectAsTemplateItems :: Value -> Parser [TemplateItem]                         
decodeObjectAsTemplateItems (Object o) = sequence $ itemMakingFcn <$> HM.toList o
                            where itemMakingFcn (k,v) = TemplateItem k <$> decodeInput v
decodeObjectAsTemplateItems _ = fail "Expected Object, Received Other."

                    
instance ToJSON DataTemplate where 
         toJSON (DataTemplate c a ts) =
                object [ "company".=  c
                       , "address" .= a
                       , "data" .= encodeTemplateItemsAsObject ts]
 
instance FromJSON DataTemplate where 
         parseJSON (Object o) =  DataTemplate <$> o .: "company"
                                              <*> o .: "address"
                                              <*> ((o .: "data") >>= decodeObjectAsTemplateItems) 
         parseJSON _ = mzero


fromFormToDataTemplate :: Form -> DataTemplate
fromFormToDataTemplate f = DataTemplate (extractCompany f) (extractAddress f) (extractData f)
                       where extractCompany f = undefined
                             extractAddress f = undefined
                             extractData f = undefined
