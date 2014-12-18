

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
{-# LA97NGUAGE OverloadedStrings #-}        
module Generators (GeneratorType (..)
                  ,generateForm) where
import           Kiosk.Backend.Form
import           Mocks.Primitive.Generators
import           Test.QuickCheck
import Data.Text (unpack)
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
generateItem gtype = do itemTypes <- generateItemTypes gtype
                        return $ Item itemTypes []

generateItems :: GeneratorType -> Gen [Item]
generateItems gtype = listOf $ generateItem gtype
-- | ItemType Generator    

generateItemTypes :: GeneratorType -> Gen [ItemType]   
generateItemTypes gtype = do lbls <- listOf $ generateLabel gtype
                             inputs <- listOf $ generateInput gtype
                             buttons <- listOf $ generateButton gtype
                             empties <- listOf $ generateEmptyBlock gtype
                             tabletops <- listOf $ generateTableTopHeader gtype
                             tablelefts <- listOf $ generateTableLefts gtype
                             return . concat $[ItemLabel <$> lbls
                                             ,ItemInput <$> inputs
                                             ,ItemButton <$> buttons
                                             ,ItemEmptyBlock <$> empties
                                             ,ItemTableTopHeader <$> tabletops
                                             ,ItemTableLeftHeader <$> tablelefts ]
-- | Label Generator

generateLabel gtype =  flip Label [] . head <$>
                       generateTexts gtype

-- | Input Generator
generateInput gtype = flip Input [] . head <$>
                      generateInputType gtype

generateInputType :: GeneratorType -> Gen [InputType]
generateInputType gtype = do
          inputTexts <- generateInputText gtype
          inputSigs  <- generateInputSignatures gtype
          return . concat $ [ InputTypeText <$> inputTexts
                            , InputTypeSignature <$> inputSigs ]

generateInputText :: GeneratorType -> Gen [InputText]
generateInputText gtype = fmap InputText  
                          <$> generateTexts gtype


generateInputSignatures gtype = do txts <- generateTexts gtype
                                   return $ Signature <$> txts

-- | Button Generator
generateButton gtype = flip Button [] . head <$>
                       generateTexts gtype
-- | EmptyBlock Generator
generateEmptyBlock gtype = return Null

-- | TableTopHeader
generateTableTopHeader gtype = TableTopHeader . head <$>
                               generateTexts gtype
-- | TableLeftHeader   
generateTableLefts gtype = TableLeftHeader . head <$>
                           generateTexts gtype
