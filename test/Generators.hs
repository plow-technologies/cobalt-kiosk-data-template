{- |
Module      :  Generators
Description :  Generators for Forms and other cabal parts
Copyright   :  Plow Technologies LLC
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

Create Test forms for use in modules
Uses regex-genex to give these forms a more realistic look

-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Generators (GeneratorType (..)
                  ,generateForm
                  ,generateDataTemplateEntry
                  ,generateDataTemplate
                  ,checkStaticGeneratorConsistency
                  ,generatePrettyTexts
                  ,genCompany
                  ,generateCompany) where
import           Control.Monad                   (ap)
import           Control.Applicative             ((<$>), (<*>))
import qualified Data.Traversable as TRVS        (sequenceA)
import           Data.Aeson                      (toJSON)
import           Data.List                       (nub)
import qualified Data.Text                       as T
import           Data.UUID                       (nil)
import           Kiosk.Backend.Data              (DataTemplateEntry (..),
                                                  DataTemplateEntryKey (..),
                                                  TicketId (..))
import           Kiosk.Backend.Data.DataTemplate (DataTemplate (..),
                                                  TemplateItem (..))
import           Kiosk.Backend.Form
import           Mocks.Primitive.Generators      (GeneratorType (..),
                                                  generateInts)
import           Test.QuickCheck


import           Regex.Genex
-- | Form Generator

-- instance Arbitrary DataTemplateEntry where
--   arbitrary = generateDataTemplateEntry

generateForm :: GeneratorType -> Gen [Form]
generateForm gtype = do companies <- generateCompany gtype
                        addresses <- generateAddress gtype
                        logos <- generateLogo gtype
                        phones <- generatePhone gtype
                        rows <- generateRows gtype
                        constants <- generateConstants gtype
                        return $ Form <$> companies
                                      <*> addresses
                                      <*> logos
                                      <*> phones
                                      <*> [constants]
                                      <*> [rows] -- second paren because it actually needs to be a list


-- | Company Generator
generateCompany :: GeneratorType -> Gen [Company]
generateCompany _ = do ctxt <- generatePrettyTexts "Cobalt|Rockshore"
                       return $ Company <$> ctxt <*> [[]]

genCompany :: GeneratorType -> Gen Company
genCompany _ = do
  ctxt <- generatePrettyTexts "Cobalt|Rockshore"
  Company `fmap` elements ctxt `ap` return []
                       
-- | Address Generator
generateAddress :: GeneratorType -> Gen [Address]
generateAddress _ = do atxt <- generatePrettyTexts "1114 Here we GO dr."
                       return $ Address <$> atxt <*> [[]]

-- | Address Generator
genAddress :: GeneratorType -> Gen Address
genAddress _ = do
  atxt <- generatePrettyTexts "1114 Here we GO dr."
  Address `fmap` elements atxt `ap` return []
                       
-- | Logo Generator
generateLogo :: GeneratorType -> Gen [Logo]
generateLogo _ = do atxt <- generatePrettyTexts "Rockshore.logo"
                    return $ Logo <$> atxt <*> [[]]

-- | Phone Generator
generatePhone :: GeneratorType -> Gen [Phone]
generatePhone _ = do atxt <- generatePrettyTexts "[1-9]{3}-[1-9]{3}-[0-9]{4}"
                     return $ Phone <$> atxt <*> [[]]


-- | Row Generator
generateRow :: GeneratorType -> Gen Row
generateRow gtype = do items <- generateItems gtype
                       return $ Row items []

generateRows :: GeneratorType -> Gen [Row]
generateRows Dynamic = listOf $ generateRow Dynamic
generateRows Static = staticListOf 5 <$> generateRow Static

-- | Constants Generator
generateConstants :: GeneratorType -> Gen [Constant]
generateConstants Dynamic =  listOf $ generateConstant Dynamic
generateConstants Static = staticListOf 5 <$> generateConstant Static

generateConstant :: GeneratorType -> Gen Constant
generateConstant Dynamic = (flip Constant []) <$> (return "Test")


generateConstant Static = return $ Constant "Company" []
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
generateLabel _ =  (fmap (flip Label []))  <$>
                        generatePrettyTexts "Some Sort OF Label [0-9]"

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
generateInputText _ = fmap InputText
                           <$> generatePrettyTexts "Some Input Text "

generateInputSignatures :: GeneratorType -> Gen [Signature]
generateInputSignatures _ = do txts <- generatePrettyTexts "Signature Block"
                               return $ Signature <$> txts

-- | Button Generator
generateButton :: GeneratorType -> Gen [Button]
generateButton _ = (fmap (flip Button [] )) <$>
                        generatePrettyTexts  "Button text"

-- | EmptyBlock Generator
generateEmptyBlock :: Monad m => t -> m EmptyBlock
generateEmptyBlock _gtype = return Null

-- | TableTopHeader
generateTableTopHeader :: GeneratorType -> Gen [TableTopHeader]
generateTableTopHeader _ = fmap TableTopHeader <$>
                                  generatePrettyTexts "HeaerText"
-- | TableLeftHeader
generateTableLefts :: GeneratorType -> Gen [TableLeftHeader]
generateTableLefts _ = fmap TableLeftHeader  <$>
                            generatePrettyTexts "LeftTableLabel"


-- | DataTemplate Generators
generatePrettyTexts :: String -> Gen [T.Text]
generatePrettyTexts str = return $ T.pack <$> genexPure [str]

-- | TemplateItem Generator
generateTemplateItem :: GeneratorType -> Gen [TemplateItem]
generateTemplateItem gtype = ((<*>).fmap TemplateItem) <$>  generatePrettyTexts "item [0-7]"
                                                          <*>  generateInputType gtype



generateTemplateItems :: GeneratorType -> Gen [TemplateItem]
generateTemplateItems Dynamic = generateTemplateItem Dynamic
generateTemplateItems Static = take 10 <$> generateTemplateItem Static

generateTicketId :: GeneratorType -> Gen TicketId
generateTicketId gtype = do
   n1 <- generateInts gtype
   n2 <- generateInts gtype
   return $ TicketId (head n1, head n2)

-- | DataTemplateEntryKey
generateDataTemplateEntryKey :: GeneratorType -> Gen [DataTemplateEntryKey]
generateDataTemplateEntryKey gtype = do
                             dfromId <- generateInts gtype
                             dfDate <- generateInts gtype
                             ticketIds <- listOf $ generateTicketId gtype
                             return $ DataTemplateEntryKey <$> dfromId
                                                           <*> [nil]
                                                           <*> ticketIds
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

instance Arbitrary [DataTemplateEntry] where
  arbitrary = generateDataTemplateEntry Dynamic 



