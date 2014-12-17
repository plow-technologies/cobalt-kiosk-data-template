{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Kiosk.Backend.Data.DataTemplate (fromFormToDataTemplate
                          ,TemplateItem(..)
                          ,DataTemplate(..)
                           ) where

import Kiosk.Backend.Form (Item(..)
                          ,ItemType(..)
                          ,Label(..)
                          ,Form(..)
                          ,Row(..)
                          ,Company (..)
                          ,Address(..)
                          ,Input(..)
                          ,InputType
                          , defaultCompany
                          , defaultForm)
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
import Data.ByteString.Lazy.Internal (ByteString)

-- Data Template Type
data DataTemplate = DataTemplate { company::Company,
                                   address :: Address, 
                                   templateItems :: [TemplateItem]}
                    
data TemplateItem = TemplateItem {
            label :: Text
            , templateValue :: InputType } deriving (Show)


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

-- decodeObjectAsInputType (String s) = itemMakingFcn s
--                         where itemMakingFcn v = InputTypeText . InputText
                                
-- Decode Input Function                                                                
decodeInput :: Value -> Parser InputType
decodeInput = parseJSON 

-- Decode Object Function
decodeObjectAsTemplateItems :: Value -> Parser [TemplateItem]                         
decodeObjectAsTemplateItems (Object o) = sequence $ itemMakingFcn <$> HM.toList o
                            where itemMakingFcn (k,v) = TemplateItem k <$> decodeInput v
decodeObjectAsTemplateItems _ = fail "Expected Object, Received Other."

-- Decode Company
decodeStringAsCompany :: Value -> Parser Company
decodeStringAsCompany (String s) = return $ itemMakingFcn s
                        where itemMakingFcn v = Company v []
decodeStringAsCompany _ = fail "Expected String, Received Other."

-- Decode Address
decodeStringAsAddress :: Value -> Parser Address
decodeStringAsAddress (String s) = return $ itemMakingFcn s
                       where itemMakingFcn v = Address v []
decodeStringAsAddress _ = fail "Expected String, Received Other."


instance ToJSON DataTemplate where 
         toJSON (DataTemplate c a ts) =
                object [ "company".=  c
                       , "address" .= a
                       , "data" .= encodeTemplateItemsAsObject ts]
 
instance FromJSON DataTemplate where 
         parseJSON (Object o) =  DataTemplate <$> ((o .: "company") >>= decodeStringAsCompany)
                                              <*> ((o .: "address" ) >>= decodeStringAsAddress)
                                              <*> ((o .: "data") >>= decodeObjectAsTemplateItems) 
         parseJSON _ = mzero

-- Type for tranform function
data ArgConstructor a b c = EmptyItem (a -> b -> c) | OneArgument (b -> c) | FullItem c
type TemplateItemConstructor = ArgConstructor Text InputType TemplateItem

-- Lens
makePrisms ''ArgConstructor

-- Function to convert Form to DataTemplate
fromFormToDataTemplate :: Form -> DataTemplate
fromFormToDataTemplate (Form c a rs)  = DataTemplate c a (extractData rs)
                       where extractData :: [Row] -> [TemplateItem]
                             extractData rows = rows ^.. traverse.rowItem.traverse.item.folding itemMakerFcn
                             itemMakerFcn :: [ItemType] -> [TemplateItem]
                             itemMakerFcn  its = foldl' sequencingFunction [EmptyItem TemplateItem] its ^.. folded._FullItem
                             sequencingFunction :: [TemplateItemConstructor] -> ItemType -> [TemplateItemConstructor]
                             sequencingFunction (EmptyItem f:items) (ItemLabel (Label l _) ) = OneArgument (f l):items
                             sequencingFunction (OneArgument f:items) (ItemInput (Input i _ )) = FullItem (f i):items
                             sequencingFunction (OneArgument _:items) _ = items
                             sequencingFunction items _ = items


testCompany :: ByteString
testCompany = "{\"company\":\"testCompany\"}"

testData :: ByteString
testData = "{\"Name_of_Lease_Operator_1\":\"Scott\",\"Field_Name_1\":\"Ling's Oilfield\",\"Flowback_Water_1\":10,\"Lease_Name_1\":\"Lease\",\"Water_Hauling_Permit_1\":5678,\"BBLS_Produced_Water_1\":100,\"Other_1\":\"Notes notes notes\",\"Fresh_Water_1\":10,\"Name_of_Water_Hauling_Company_1\":\"Plowtech Hauling\",\"Date_1\":\"12/12/2014\",\"Drivers_Signature_1\":\"James Haver\",\"Truck_1\":\"1234\",\"Time_in_1\":\"Yesterday\",\"Pit_Water_1\":20}"

testJSON :: ByteString
testJSON = "{\"data\":{\"Name_of_Lease_Operator_1\":\"Scott\",\"Field_Name_1\":\"Ling's Oilfield\",\"Flowback_Water_1\":10,\"Lease_Name_1\":\"Lease\",\"Water_Hauling_Permit_1\":5678,\"BBLS_Produced_Water_1\":100,\"Other_1\":\"Notes notes notes\",\"Fresh_Water_1\":10,\"Name_of_Water_Hauling_Company_1\":\"Plowtech Hauling\",\"Date_1\":\"12/12/2014\",\"Drivers_Signature_1\":\"James Haver\",\"Truck_1\":\"1234\",\"Time_in_1\":\"Yesterday\",\"Pit_Water_1\":20},\"company\":\"testCompany\",\"address\":\"testAddress\"}"

fromJSONToDataTemplate :: ByteString -> Either String DataTemplate
fromJSONToDataTemplate bs = eitherDecode bs :: Either String DataTemplate



