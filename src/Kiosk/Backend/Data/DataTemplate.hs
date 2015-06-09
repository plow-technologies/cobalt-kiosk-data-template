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
                                       , getConstants
                                       , getLogo
                                       , getPhone
                                       , labelAttrib
                                       , getInput
                                       , inputAttrib
                                       , checkType
                                       , decodeStringAsCompany
                                       , decodeStringAsAddress
                                       , fitDataTemplateToForm
                                       , getAddress) where
import           Data.Aeson                    (FromJSON, ToJSON, Value (..),
                                                eitherDecode, object, parseJSON,
                                                toJSON, (.=))
import           Data.Aeson.Types              (Parser)
import           Data.ByteString.Lazy.Internal (ByteString)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Kiosk.Backend.Form            (Address (..), Company (..),
                                                Form (..), Input (..),
                                                InputDate (..),
                                                InputDouble (..), InputInt (..),
                                                InputText (..), InputTime (..),
                                                InputType (..), InputType (..),
                                                Item (..), ItemType (..),
                                                Label (..), Row (..),
                                                Signature (..))

import           Control.Applicative           ((<$>), (<|>))

import           Control.Lens                  (folded, folding, makeClassy_,
                                                makeLenses, makePrisms,
                                                traverse, (^..))
import           Data.Monoid                   ((<>))
-- import           Control.Monad                 (mzero)
import qualified Data.Csv                      as C
import           Data.Foldable                 (foldl')
import qualified Data.HashMap.Strict           as HM
import           Data.Map                      (Map)
import qualified Data.Map                      as M
import qualified Data.Traversable              as Traversable
import           Data.Typeable
import qualified Data.Vector                   as V

-- Data Template Type
newtype DataTemplate = DataTemplate {
                                        templateItems :: [TemplateItem]}
                                        deriving (Ord,Eq,Show)

instance C.ToRecord DataTemplate where
  toRecord (DataTemplate ts) = V.fromList $ C.toField <$> ts

-- instance Tabular DataTemplate where

data TemplateItem = TemplateItem {
              label         :: Text
            , templateValue :: InputType }
            deriving (Show,Ord,Eq)

instance C.ToField TemplateItem where
  toField (TemplateItem _ (InputTypeText (InputText t))) = C.toField t
  toField (TemplateItem _ (InputTypeInt (InputInt i))) = C.toField i
  toField (TemplateItem _ (InputTypeDouble (InputDouble d))) = C.toField d
  toField (TemplateItem _ (InputTypeSignature (Signature s))) = C.toField s
  toField (TemplateItem _ (InputTypeDate (InputDate d))) = C.toField d
  toField (TemplateItem _ (InputTypeTime (InputTime t))) = C.toField t

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
                       codeAsInputType (InputTypeDate (InputDate s)) = toJSON s
                       codeAsInputType (InputTypeTime (InputTime t)) = toJSON t
                       labelIncrementor templateItems' = replaceOldLabels templateItems' .
                                                          makeTexts $ templateItems'
                       makeTexts = fmap label
                       replaceOldLabels = zipWith (\ti l -> ti {label = l})


-- | Decode Input Function
-- This does decode the input function but it should be noted that the type signature basically never gets called b
-- because in javascript form it looks identical to the InputText field
decodeInput :: Value -> Parser InputType
decodeInput v = InputTypeText . InputText  <$> parseJSON v  <|>
                InputTypeSignature . Signature <$> parseJSON v <|>
                InputTypeInt . InputInt <$> parseJSON v        <|>
                InputTypeDouble . InputDouble <$> parseJSON v


-- Decode Object Function
decodeObjectAsTemplateItems :: Value -> Parser [TemplateItem]
decodeObjectAsTemplateItems (Object o) = fmap reverse . sequence $ itemMakingFcn <$> HM.toList o
                            where itemMakingFcn (k,v) = (TemplateItem  k) <$> decodeInput v
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
         toJSON (DataTemplate ts) = encodeTemplateItemsAsObject ts

instance FromJSON DataTemplate where
         parseJSON o =  DataTemplate <$> decodeObjectAsTemplateItems o


-- Type for tranform function
data ArgConstructor a b c = EmptyItem (a -> b -> c) | OneArgument (b -> c) | FullItem c
type TemplateItemConstructor = ArgConstructor Text InputType TemplateItem

-- Lens
makePrisms ''ArgConstructor

-- | Function to convert Form to DataTemplate
-- This is mostly about stripping away the stuff that isn't an input field

fromFormToDataTemplate :: Form -> DataTemplate
fromFormToDataTemplate (Form _c _a _l _p  _constants rs)  = DataTemplate (extractData rs)
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


type LabelMap = Map Text InputType


-- | Try to fit a given DataTemplate to a given form
fitDataTemplateToForm :: Form -> DataTemplate -> Either String DataTemplate
fitDataTemplateToForm frm dt = fromLabelMap <$> fitDT dt
  where
    dtMapFromForm :: LabelMap
    dtMapFromForm = toLabelMap.fromFormToDataTemplate $ frm
    fitDT dt' =  mergeFormDTwithTargetDT dtMapFromForm . toLabelMap $ dt'
    mergeFormDTwithTargetDT :: LabelMap -> LabelMap -> Either String LabelMap
    mergeFormDTwithTargetDT referenceDTmap lmap = Traversable.sequence $ M.mapWithKey
                                                  (convertTypeIfNeeded referenceDTmap) lmap
    convertTypeIfNeeded :: LabelMap -> Text -> InputType -> Either String InputType
    convertTypeIfNeeded referenceDTMap k targetInput = maybe (Left $ "Input Type not found at" <> T.unpack k)
                                                             (typeMatchAndConvert targetInput )
                                                             (M.lookup k referenceDTMap)


-- | take two InputTypes and transform one into the other if
typeMatchAndConvert :: InputType -> InputType -> Either String InputType
typeMatchAndConvert targetInput referenceInput
 |checkType targetInput referenceInput = Right targetInput
 |otherwise = Left "Error Input Types do not match"


toLabelMap :: DataTemplate -> Map Text InputType
toLabelMap dt' = foldr createLabelMap M.empty (templateItems dt')
  where
     createLabelMap templateItem  = M.insert (label templateItem)
                (templateValue templateItem)

fromLabelMap :: Map Text InputType -> DataTemplate
fromLabelMap lmap = DataTemplate convertedMap
  where
   convertedMap = M.foldWithKey makeTemplateItem [] lmap
   makeTemplateItem lbl inputType lst = TemplateItem lbl inputType:lst
