{-# LANGUAGE TypeFamilies #-}
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
                                         , MigrationError (..)) where

import Kiosk.Backend.Data ( DataTemplateEntry(..)
                           )
import Data.Text (Text)
import Data.Either.Validation (Validation (..))


data MigrationError e i = MigrationError { getError :: e
                                         , getIncomingData :: i }


class FromDataTemplate e where
  fromDataTemplate :: DataTemplateEntry -> e

class ToDataTemplate e where
  toDataTemplate :: e -> DataTemplateEntry

class (FromDataTemplate i, ToDataTemplate o) => FormMigration i o | i -> o  where   
  transformRecord :: i -> Validation (MigrationError Text i) o







