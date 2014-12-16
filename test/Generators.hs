

{- |
Module      :  Generators
Description :  Generators for Forms and other cabal parts
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

Create Test forms for use in modules 

-}
{-# LANGUAGE OverloadedStrings #-}        
module Generators (GeneratorType (..)
                  ,generateForm) where
import           Kiosk.Backend.Form
import           Mocks.Primitive.Generators
import           Test.QuickCheck
import Control.Applicative ((<$>)
                           ,(<*>))

-- | Form Generator

generateForm :: GeneratorType -> Gen [Form]
generateForm gtype = do companies <- generateCompany gtype
                        addresses <- generateAddress gtype
                        rows <- generateRows gtype
                        return $ Form <$> companies 
                                      <*> addresses
                                      <*> [rows] -- second paren because it actually needs to be a list


-- | Company Generator 
generateCompany :: GeneratorType -> Gen [Company]
generateCompany gtype = do ctxt <- generateTexts gtype
                           return $ Company <$> ctxt <*> [[]]
-- | Address Generator 
generateAddress :: GeneratorType -> Gen [Address]
generateAddress gtype = do atxt <- generateTexts gtype
                           return $ Address <$> atxt <*> [[]]
-- | Row Generator   
generateRow :: GeneratorType -> Gen Row
generateRow gtype = do items <- generateItems gtype
                       return $ Row items []

generateRows :: GeneratorType -> Gen [Row]
generateRows gtype = listOf $ generateRow gtype
-- | Item Generator 

generateItem :: GeneratorType -> Gen Item 
generateItem gtype = do itemTypes <- generateItemTypes
                        return $ Item itemTypes []

generateItems :: GeneratorType -> Gen [Item]
generateItems gtype = listOf $ generateItem gtype
-- | ItemType Generator    
   
generateItemTypes = undefined   
-- | Label Generator
-- | Button Generator
-- | EmptyBlock Generator
-- | TableTopHeader
-- | TableLeftHeader   
