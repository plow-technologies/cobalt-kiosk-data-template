{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Kiosk.Backend.Data.DataTemplate ( fromFormToDataTemplate
                                        ,fromJSONToDataTemplate
                                        ,decodeObjectAsTemplateItems
                                        ,TemplateItem(..)
                                        ,DataTemplate(..)
                                       , checkCompanyType
                                       , getCompany
                                       , row
                                       , rowAttrib
                                       , itemAttrib
                                       , getLabelText
                                       , labelAttrib
                                       , getInput
                                       , inputAttrib
                                       , checkType
                                       , decodeStringAsCompany
                                       , decodeStringAsAddress

                                        ,getAddress) where

import Kiosk.Backend.Form (Item(..)
                          ,ItemType(..)
                          ,Label(..)
                          ,Form(..)
                          ,Row(..)
                          ,Company (..)
                          ,Address(..)
                          ,Signature (..)
                          ,Input(..)
                          ,InputType(..)
                          ,InputText(..)
                          ,InputInt(..)
                          ,InputDouble(..)
                          )
import Data.Aeson
import  Data.Aeson.Types  
import Data.Text (Text)
import Data.ByteString.Lazy.Internal (ByteString)

import Control.Applicative ((<$>), (<*>), (<|>))
import           Control.Monad             (mzero)
import qualified Data.HashMap.Strict as HM (toList)
import Data.Foldable (foldl')
import Control.Lens (makeLenses
                    ,makePrisms
                    ,traverse
                    ,folding
                    ,folded
                    ,(^..) )
import Data.Typeable



-- Data Template Type
data DataTemplate = DataTemplate { company::Company,
                                   address :: Address, 
                                   templateItems :: [TemplateItem]} 
                                   deriving (Ord,Eq,Show)

-- instance Tabular DataTemplate where

data TemplateItem = TemplateItem {
            label :: Text
            , templateValue :: InputType } deriving (Show,Ord,Eq)

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
                                            templateValue=v}) = l .= codeAsInputType v
                       codeAsInputType (InputTypeText (InputText t) ) = toJSON t
                       codeAsInputType (InputTypeSignature (Signature s)) = toJSON s
                       codeAsInputType (InputTypeInt (InputInt s)) = toJSON s                                                             
                       codeAsInputType (InputTypeDouble (InputDouble s)) = toJSON s                                                                    



-- Decode Input Function                                                                
decodeInput :: Value -> Parser InputType
decodeInput v = InputTypeText . InputText  <$> parseJSON v  <|>
                InputTypeSignature . Signature <$> parseJSON v <|>
                InputTypeInt . InputInt <$> parseJSON v        <|>     
                InputTypeDouble . InputDouble <$> parseJSON v                

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
         toJSON (DataTemplate (Company c _ ) (Address a _) ts) =
                object [ "company".=  c
                       ,  "address" .= a
                       , "data" .= encodeTemplateItemsAsObject ts]
 
instance FromJSON DataTemplate where 
         parseJSON (Object o) =  DataTemplate <$> (flip Company [] 
                                                   <$>  o .: "company"  )            
                                              <*> (flip Address []
                                                    <$> o .: "address")              
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


fromJSONToDataTemplate :: ByteString -> Either String DataTemplate
fromJSONToDataTemplate bs = eitherDecode bs :: Either String DataTemplate

checkType :: (Typeable a1, Typeable a) => a -> a1 -> Bool
checkType a b = typeOf a == typeOf b

checkCompanyType :: (Typeable a1, Typeable a) => a -> a1 -> Bool
checkCompanyType a b = checkType a b

-- validateDataTemplate (DataTemplate c1 a1 d1) (DataTemplate c2 a2 d2) =
--                                    where companyValid = checkCompanyType c1 c2
                                         
