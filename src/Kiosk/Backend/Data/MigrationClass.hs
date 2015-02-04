{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}

{- |
Module      :  Kiosk.Backend.Data.MigrationClass
Description :  Migrations from one form to another
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

Migrations take one form entry to another,
They are ad-hock but the form is set,
forms start out without a structure and are sent through 
an intermediate type with a MigrationClass created on it,
this results in a final type   

-}
module Kiosk.Backend.Data.MigrationClass ( FormMigration(..)
                                         , ToDataTemplate (..)
                                         , FromDataTemplate(..)
                                         , MigrationError (..)
                                         , extractTemplateItems
                                         , extractDataTemplateEntryKey
                                         , extractValueFromTemplateItems
                                         , makeTemplateItemDouble
                                         , makeTemplateItemInt
                                         , makeTemplateItemText) where
import           Control.Lens                      (view)
import           Kiosk.Backend.Data.DataTemplate  (TemplateItem(..)
                                                  ,templateItems) 
import Kiosk.Backend.Form.Element                 
import Kiosk.Backend.Data ( DataTemplateEntry(..)
                          , DataTemplateEntryKey(..)
                          , dataTemplateEntryKey
                          , dataTemplateEntryValue)
import GHC.Exts                          
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import Data.Either.Validation (Validation (..))


--------------------------------------------------
--------------------------------------------------        


-- |Migration Errors are used in Validations on Migrations 
data MigrationError e i = MigrationError { getError :: e
                                         , getIncomingData :: i }


-- | The 'FromDataTemplate' , 'ToDataTemplate' and 'FormMigration' typeclasses
-- should help standardize the migration interfaces between forms.   

class FromDataTemplate e where
  fromDataTemplate :: DataTemplateEntry -> e

class ToDataTemplate e where
  toDataTemplate :: e -> DataTemplateEntry

class (FromDataTemplate i, ToDataTemplate o) => FormMigration i o | i -> o  where   
  transformRecord :: i -> Validation (MigrationError Text i) o


--------------------------------------------------

-- | Utility Functions 
   
-- | Use 'extractTemplateItems to get 'TemplateItem'

extractTemplateItems :: DataTemplateEntry -> [TemplateItem]
extractTemplateItems dt  = templateItems $ view dataTemplateEntryValue dt   



newtype TemplateItemLabel = TemplateItemLabel String
  deriving (IsString,Eq,Ord,Read,Show)                                            



extractValueFromTemplateItems :: TemplateItemLabel -> [TemplateItem] -> Text
extractValueFromTemplateItems (TemplateItemLabel key) items = case L.find (\a -> label a == T.pack key) items of
                                                                        Just item -> convertInputTypeToValue item
                                                                        Nothing -> (""::Text)


extractDataTemplateEntryKey :: DataTemplateEntry -> DataTemplateEntryKey
extractDataTemplateEntryKey = view dataTemplateEntryKey

convertInputTypeToValue :: TemplateItem -> T.Text
convertInputTypeToValue = extractInputType


extractInputType :: TemplateItem -> T.Text
extractInputType (TemplateItem _ (InputTypeText (InputText t))) = t
extractInputType (TemplateItem _ (InputTypeInt (InputInt i))) = T.pack . show $ i
extractInputType (TemplateItem _ (InputTypeDouble (InputDouble d))) = T.pack . show $ d
extractInputType (TemplateItem _ (InputTypeSignature (Signature s))) = s
extractInputType (TemplateItem _ (InputTypeDate (InputDate t))) = t
extractInputType (TemplateItem _ (InputTypeTime (InputTime t))) = t



-- | Template Item Conversion functions 
makeTemplateItemDouble :: TemplateItemLabel -> Double -> TemplateItem
makeTemplateItemDouble (TemplateItemLabel lbl) value = TemplateItem (T.pack lbl) (InputTypeDouble (InputDouble value))

makeTemplateItemInt :: TemplateItemLabel -> Int -> TemplateItem
makeTemplateItemInt (TemplateItemLabel lbl) value = TemplateItem (T.pack lbl) (InputTypeInt (InputInt value))

makeTemplateItemText :: TemplateItemLabel -> T.Text -> TemplateItem
makeTemplateItemText (TemplateItemLabel lbl) value = TemplateItem (T.pack lbl) (InputTypeText (InputText value))
