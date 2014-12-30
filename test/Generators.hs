

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
                  ,generateForm
                  ,generateDataTemplateEntry
                  ,generateDataTemplate
                  ,checkStaticGeneratorConsistency) where
import           Control.Applicative             ((<$>), (<*>))
import           Data.Aeson                      (toJSON)
import           Data.List                       (nub)
import           Data.UUID                       (nil)
import           Kiosk.Backend.Data              (DataTemplateEntry (..),
                                                  DataTemplateEntryKey (..))
import           Kiosk.Backend.Data.DataTemplate (DataTemplate (..),
                                                  TemplateItem (..))
import           Kiosk.Backend.Form
import           Mocks.Primitive.Generators      (GeneratorType (..),
                                                  generateInts, generateTexts)
import           Test.QuickCheck

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
generateRows Dynamic = listOf $ generateRow Dynamic
generateRows Static = staticListOf 5 <$> generateRow Static
-- | Item Generator

generateItem :: GeneratorType -> Gen Item
generateItem gtype = do itemTypes <- generateItemTypes gtype
                        return $ Item itemTypes []

generateItems :: GeneratorType -> Gen [Item]
generateItems Dynamic = listOf $ generateItem Dynamic
generateItems Static = staticListOf 5  <$>  generateItem Static

              -- | ItemType Generator
staticListOf :: Int -> a -> [a]
staticListOf n = take n . repeat

generateItemTypes :: GeneratorType -> Gen [ItemType]
generateItemTypes Dynamic = do lbls <- generateLabel Dynamic
                               inputs <- generateInput Dynamic
                               buttons <-  generateButton Dynamic
                               empties <- listOf $ generateEmptyBlock Dynamic
                               tabletops <-  generateTableTopHeader Dynamic
                               tablelefts <- generateTableLefts Dynamic
                               return . concat $[ItemLabel <$> lbls
                                                    ,ItemInput <$> inputs
                                                    ,ItemButton <$> buttons
                                                    ,ItemEmptyBlock <$> empties
                                                    ,ItemTableTopHeader <$> tabletops
                                                    ,ItemTableLeftHeader <$> tablelefts ]
generateItemTypes Static = do lbls <- take 5 <$> generateLabel Static
                              inputs <- take 5 <$> generateInput Static
                              buttons <- take 5 <$> generateButton Static
                              empties <- staticListOf 5 <$> generateEmptyBlock Static
                              tabletops <- take 5 <$> generateTableTopHeader Static
                              tablelefts <- take 5 <$> generateTableLefts Static
                              return . concat $[ItemLabel <$> lbls
                                               ,ItemInput <$> inputs
                                               ,ItemButton <$> buttons
                                               ,ItemEmptyBlock <$> empties
                                               ,ItemTableTopHeader <$> tabletops
                                               ,ItemTableLeftHeader <$> tablelefts ]
-- | Label Generator

generateLabel :: GeneratorType -> Gen [Label]
generateLabel gtype =  (fmap (flip Label []))  <$>
                       generateTexts gtype

-- | Input Generator
generateInput :: GeneratorType -> Gen [Input]
generateInput gtype = (fmap  (flip Input [])) <$>
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

generateInputSignatures :: GeneratorType -> Gen [Signature]
generateInputSignatures gtype = do txts <- generateTexts gtype
                                   return $ Signature <$> txts

-- | Button Generator
generateButton :: GeneratorType -> Gen [Button]
generateButton gtype = (fmap (flip Button [] )) <$>
                       generateTexts gtype

-- | EmptyBlock Generator
generateEmptyBlock :: Monad m => t -> m EmptyBlock
generateEmptyBlock _gtype = return Null

-- | TableTopHeader
generateTableTopHeader :: GeneratorType -> Gen [TableTopHeader]
generateTableTopHeader gtype = fmap TableTopHeader <$>
                                 generateTexts gtype
-- | TableLeftHeader
generateTableLefts :: GeneratorType -> Gen [TableLeftHeader]
generateTableLefts gtype = fmap TableLeftHeader  <$>
                           generateTexts gtype


-- | DataTemplate Generators


-- | TemplateItem Generator
generateTemplateItem :: GeneratorType -> Gen [TemplateItem]
generateTemplateItem gtype = ((<*>).fmap TemplateItem) <$>  generateTexts gtype
                                                          <*>  generateInputType gtype

generateTemplateItems :: GeneratorType -> Gen [TemplateItem]
generateTemplateItems Dynamic = generateTemplateItem Dynamic
generateTemplateItems Static = take 10 <$> generateTemplateItem Static

-- | DataTemplateEntryKey
generateDataTemplateEntryKey :: GeneratorType -> Gen [DataTemplateEntryKey]
generateDataTemplateEntryKey gtype = do
                             dfromId <- generateInts gtype
                             dfDate <- generateInts gtype
                             return $ DataTemplateEntryKey <$> dfromId
                                                           <*> [nil]
                                                           <*> dfDate

-- | DataTemplate Generator
generateDataTemplate :: GeneratorType -> Gen [DataTemplate]
generateDataTemplate gtype = do
                     tData <- generateTemplateItems gtype
                     return $ DataTemplate <$> [tData]

-- | DataTemplateEntry Generator
generateDataTemplateEntry :: GeneratorType -> Gen [DataTemplateEntry]
generateDataTemplateEntry gtype = do
                           dEntryKey <- generateDataTemplateEntryKey gtype
                           dEntry <- generateDataTemplate gtype
                           return . nub $ DataTemplateEntry <$> dEntryKey
                                                                             <*> dEntry


checkStaticGeneratorConsistency :: Int -> Gen Bool
checkStaticGeneratorConsistency i = let x = (take i) <$> (generateDataTemplateEntry Static)
                                        y =  (take i) <$> (generateDataTemplateEntry Static)
                                    in (==) <$> (toJSON <$> x )
                                            <*> (toJSON <$> y)
