{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Kiosk.Backend.Data (fromFormToDataTemplate
                          ,TemplateItem(..)
                          ,DataTemplate(..)
                          ,) where

import Kiosk.Backend.Form (Item(..)
                          ,ItemType(..)
                          ,Label(..)
                          ,Form(..)
                          ,Row(..)
                          ,Company (..)
                          ,Address(..)
                          ,Input(..)
                          ,InputType)
import Data.Aeson
import  Data.Aeson.Types  
import Data.Text (Text)

import Control.Applicative ((<$>), (<*>) )
import           Control.Monad             (mzero)
import qualified Data.HashMap.Strict as HM (toList)
import Data.Foldable (foldl')
import Control.Lens (makeLenses
                    ,makePrisms
                    ,traverse
                    ,folding
                    ,folded
                    ,(^..) )

-- Data Template Type
data DataTemplate = DataTemplate { company::Company,
                                   address :: Address, 
                                   templateItems :: [TemplateItem]}
data TemplateItem = TemplateItem {
            label :: Text
            , templateValue :: InputType }

-- Make Lenses

makeLenses ''Form
makeLenses ''Row             
makeLenses ''Item
makeLenses ''Input                        
makeLenses ''Label           
makePrisms ''ItemType                                    
makeLenses ''DataTemplate            


-- JSON Instances
-- | encode a list of items as a flat single object instead of as an array of objects
encodeTemplateItemsAsObject :: [TemplateItem] -> Value
encodeTemplateItemsAsObject items = object $ fmap objectMaker items
                      where
                       objectMaker (TemplateItem { label=l,
                                            templateValue=v}) = l .= v
decodeInput :: Value -> Parser InputType
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


data ArgConstructor a b c = EmptyItem (a -> b -> c) | OneArgument (b -> c) | FullItem c
type TemplateItemConstructor = ArgConstructor Text InputType TemplateItem

makePrisms ''ArgConstructor                                                                                             
           
fromFormToDataTemplate :: Form -> DataTemplate
fromFormToDataTemplate (Form c a rs)  = DataTemplate c a (extractData rs)
                       where extractData :: [Row] -> [TemplateItem]
                             extractData rows = rows ^.. traverse.rowItem.traverse.item.folding itemMakerFcn
                             itemMakerFcn :: [ItemType] -> [TemplateItem]
                             itemMakerFcn  its = foldl' sequencingFunction [EmptyItem TemplateItem] its ^.. folded._FullItem
                             sequencingFunction :: [TemplateItemConstructor] -> ItemType -> [TemplateItemConstructor]
                             sequencingFunction (EmptyItem f:items) (ItemLabel (Label l _) ) = OneArgument (f l):items
                             sequencingFunction (OneArgument f:items) (ItemInput (Input i _ )) = FullItem (f i):items
                             sequencingFunction (OneArgument f:items) _ = items
                             sequencingFunction items _ = items


