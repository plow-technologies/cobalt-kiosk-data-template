{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Kiosk.Backend.Data.DataTemplate ( fromFormToDataTemplate
                                        ,fromJSONToDataTemplate
                                        ,fromDataTemplateToCSV
                                        ,decodeObjectAsTemplateItems
                                        ,TemplateItem(..)
                                        ,InputType (..)
                                        ,InputText (..)
                                        ,DataTemplate(..)
                                       , _templateItems
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


import           Data.Aeson                    (FromJSON, ToJSON, Value (..),
                                                eitherDecode, object, parseJSON,
                                                toJSON, (.:), (.=))
import           Data.Aeson.Types              (Parser)
import           Data.Attoparsec.Text          (char, decimal, parseOnly,
                                                takeText)
import           Data.ByteString.Lazy.Internal (ByteString)
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Kiosk.Backend.Form            (Address (..), Company (..),
                                                Form (..), Input (..),
                                                InputDouble (..), InputInt (..),
                                                InputText (..), InputType (..),
                                                Item (..), ItemType (..),
                                                Label (..), Row (..),
                                                Signature (..))

import           Control.Applicative           ((*>), (<$>), (<|>))
import           Control.Lens                  (folded, folding, makeClassy_,
                                                makeLenses, makePrisms,
                                                traverse, (^..))
import           Control.Monad                 (mzero)
import qualified Data.Csv                      as C
import           Data.Foldable                 (foldl')
import qualified Data.HashMap.Strict           as HM
import           Data.Typeable
import qualified Data.Vector                   as V


-- Data Template Type
data DataTemplate = DataTemplate {
                                   templateItems :: [TemplateItem]}
                                   deriving (Ord,Eq,Show)

instance C.ToRecord DataTemplate where
  toRecord (DataTemplate ts) = V.fromList $ C.toField <$> ts

-- instance Tabular DataTemplate where

data TemplateItem = TemplateItem {
              label         :: Text
            , templateValue :: InputType } deriving (Show,Ord,Eq)

instance C.ToField TemplateItem where
  toField (TemplateItem _ (InputTypeText (InputText t))) = C.toField t
  toField (TemplateItem _ (InputTypeInt (InputInt i))) = C.toField i
  toField (TemplateItem _ (InputTypeDouble (InputDouble d))) = C.toField d
  toField (TemplateItem _ (InputTypeSignature (Signature s))) = C.toField s


instance C.FromField TemplateItem where

-- Make Lenses
makeLenses ''Form
makeLenses ''Row
makeLenses ''Item
makeLenses ''Input
makeLenses ''Label
makePrisms ''ItemType
makeClassy_ ''DataTemplate


-- JSON Instances
-- | encode a list of items as a flat single object instead of as an array of objects
encodeTemplateItemsAsObject :: [TemplateItem] -> Value
encodeTemplateItemsAsObject items = object (objectMaker <$> labelIncrementor items)
                      where
                       objectMaker (TemplateItem { label=l,
                                            templateValue=v}) = l .= codeAsInputType v
                       codeAsInputType (InputTypeText (InputText t) ) = toJSON t
                       codeAsInputType (InputTypeSignature (Signature s)) = toJSON s
                       codeAsInputType (InputTypeInt (InputInt s)) = toJSON s
                       codeAsInputType (InputTypeDouble (InputDouble s)) = toJSON s
                       labelIncrementor templateItems' = replaceOldLabels templateItems' .
                                                          makeUniqueLabels AppendUnderScoredNumber .
                                                          makeTexts $ templateItems'
                       makeTexts = fmap label
                       replaceOldLabels templateItems' labels = zipWith (\ti l -> ti {label = l}) templateItems' labels


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
                                                                             in ( HM.insert incomingLabel i' labelMap , (incomingLabel <> "_" <> (T.pack.show $ i')):transformedLabels)

unmakeUniqueLabels :: Appender -> Text -> Text
unmakeUniqueLabels AppendUnderScoredNumber = pullOffAppender
  where
    _underScorePlusNumberLength = 2::Int
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
decodeObjectAsTemplateItems (Object o) = fmap reverse . sequence $ itemMakingFcn <$> HM.toList o
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
         toJSON (DataTemplate ts) =
                object [ "data" .= encodeTemplateItemsAsObject ts]

instance FromJSON DataTemplate where
         parseJSON (Object o) =  DataTemplate <$> ((o .: "data") >>= decodeObjectAsTemplateItems)
         parseJSON _ = mzero

-- Type for tranform function
data ArgConstructor a b c = EmptyItem (a -> b -> c) | OneArgument (b -> c) | FullItem c
type TemplateItemConstructor = ArgConstructor Text InputType TemplateItem

-- Lens
makePrisms ''ArgConstructor

-- Function to convert Form to DataTemplate
fromFormToDataTemplate :: Form -> DataTemplate
fromFormToDataTemplate (Form _c _a rs)  = DataTemplate (extractData rs)
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


fromDataTemplateToCSV :: C.ToRecord a => [a] -> ByteString
fromDataTemplateToCSV =  C.encode

checkType :: (Typeable a1, Typeable a) => a -> a1 -> Bool
checkType a b = typeOf a == typeOf b

checkCompanyType :: (Typeable a1, Typeable a) => a -> a1 -> Bool
checkCompanyType = checkType

-- validateDataTemplate (DataTemplate c1 a1 d1) (DataTemplate c2 a2 d2) =
--                                    where companyValid = checkCompanyType c1 c2
