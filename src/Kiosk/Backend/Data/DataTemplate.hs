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
                                       -- Label Uniqueness Functions
                                       , Appender(..)
                                       , makeUniqueLabels
                                       , unmakeUniqueLabels
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
import Data.Aeson (ToJSON 
                  ,FromJSON
                  ,toJSON
                  ,parseJSON
                  ,Value(..)
                  ,object
                  ,(.=)                    
                  ,(.:)
                  ,eitherDecode)
import  Data.Aeson.Types  (Parser)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Attoparsec.Text (parseOnly
                             ,decimal
                             ,char
                             ,takeText
                             ,anyChar
                             ,atEnd)

import Control.Applicative ((<$>), (<*>), (<|>),(*>),(<*))
import           Control.Monad             (mzero)
import qualified Data.HashMap.Strict as HM 
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
encodeTemplateItemsAsObject items = object $ fmap objectMaker $ labelIncrementor  items
                      where
                       objectMaker (TemplateItem { label=l,
                                            templateValue=v}) = l .= codeAsInputType v                                                                                     
                       codeAsInputType (InputTypeText (InputText t) ) = toJSON t
                       codeAsInputType (InputTypeSignature (Signature s)) = toJSON s
                       codeAsInputType (InputTypeInt (InputInt s)) = toJSON s                                                             
                       codeAsInputType (InputTypeDouble (InputDouble s)) = toJSON s                                                                    
                       labelIncrementor templateItems = replaceOldLabels templateItems . 
                                                          makeUniqueLabels AppendUnderScoredNumber . 
                                                          makeTexts $ templateItems
                       makeTexts = fmap label
                       replaceOldLabels templateItems labels = zipWith (\ti l -> ti {label = l}) templateItems labels


data Appender = AppendUnderScoredNumber
  deriving (Eq,Ord,Show)     


{- | labels need to be indexed so they can be decoded correctly: 
   label, label -> label_1 label_2 -}
makeUniqueLabels :: Appender -> [Text] -> [Text]
makeUniqueLabels AppendUnderScoredNumber incoming = reverse.snd $ foldl' appender (HM.empty,[]) incoming
        where 
          appender (labelMap,transformedLabels) incomingLabel = case HM.lookup incomingLabel labelMap of
                                                                  Nothing -> ( HM.insert incomingLabel 1 labelMap , T.append incomingLabel "_1":transformedLabels)
                                                                  (Just i) ->let i' = succ i
                                                                             in ( HM.insert incomingLabel i' labelMap , (incomingLabel <> "_" <> (T.pack.show $ i)):transformedLabels)

unmakeUniqueLabels :: Appender -> Text -> Text
unmakeUniqueLabels AppendUnderScoredNumber incoming = pullOffAppender incoming
  where
    underScorePlusNumberLength = 2
    pullOffAppender = parseReversedUnderscoreIncrementor

parseReversedUnderscoreIncrementor :: Text -> Text  
parseReversedUnderscoreIncrementor txt  = replaceTextWithReversedOriginalOnLeft . reverseTextThenParse $ txt 

       where reverseParser = decimal *> 
                             char '_' *> takeText
             reverseTextThenParse txt' = parseOnly reverseParser (T.reverse txt')
             replaceTextWithReversedOriginalOnLeft (Left _) = txt
             replaceTextWithReversedOriginalOnLeft (Right t) = T.reverse t

-- --------------------------------------------------
-- Decode Input Function 
decodeInput :: Value -> Parser InputType
decodeInput v = InputTypeText . InputText  <$> parseJSON v  <|>
                InputTypeSignature . Signature <$> parseJSON v <|>
                InputTypeInt . InputInt <$> parseJSON v        <|>     
                InputTypeDouble . InputDouble <$> parseJSON v                

-- Decode Object Function
decodeObjectAsTemplateItems :: Value -> Parser [TemplateItem]                         
decodeObjectAsTemplateItems (Object o) = sequence $ itemMakingFcn <$> HM.toList o
                            where itemMakingFcn (k,v) = (TemplateItem . unmakeUniqueLabels AppendUnderScoredNumber $ k) <$> decodeInput v
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
                                         
