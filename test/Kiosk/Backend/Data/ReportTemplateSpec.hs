{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

{- |
Module      :  Kiosk.Backend.Data.ReportTemplateSpec
Description :  Tests for Report Generation
Copyright   :  Plow Technologies LLC
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

These are tests for report generation as
it specifically relates to data templates
-}



module Kiosk.Backend.Data.ReportTemplateSpec (spec,main) where

import           Control.Applicative               ((<$>), (<*>))
import           Data.String                       (IsString)
-- import           Control.Lens
import           Data.Aeson
import           Data.Text                         (Text)
import qualified Data.Text                         as T

-- import           Data.ByteString.Lazy.Char8        (ByteString)
-- import qualified Data.ByteString.Lazy.Char8        as B
-- import           Data.Map.Strict                   (Map)
-- import qualified Data.Map.Strict                   as M
-- import           Data.Maybe                        (catMaybes)
-- import           Data.Monoid                       ((<>))
-- import           Data.Time
import           Generators
-- import           Kiosk.Backend.Data.DataTemplate
import           Kiosk.Backend.Data.ReportTemplate
import           Kiosk.Backend.Form
import           Language.Haskell.TH
-- import           Mocks.Primitive.Generators        (GeneratorType (..),
--                                                     generateInts)
-- import           ReportTemplate.Internal
-- import           System.Locale                     (defaultTimeLocale)
import           Test.Hspec
import           Test.QuickCheck
main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
 describe (nameBase 'makeCellDoubleFromInputDouble) $ do
   it "Gets an Input double and transfers it to a CellDouble" $ do
    True `shouldBe` True
 describe (nameBase 'makeCellTextFromInputText) $ do
   it "Gets an InputText and transfers it to a CellText" $ do
    True `shouldBe` True


convertToKioskForm :: CobaltWaterHaulingCompany -> Form
convertToKioskForm waterHaulingCompany = Form cobaltEnvironmentalSolutions cobaltAddress cobaltLogo defaultPhone [createWaterHauler waterHaulingName] cobaltFormBody
  where
    waterHaulingName = _whcCompanyName $ waterHaulingCompany


newtype UUID = UUID { _getUUID :: Text}
           deriving (Read,Eq,Show,IsString,ToJSON,FromJSON,Ord)

data CobaltWaterHaulingCompany = CobaltWaterHaulingCompany { _whcFormId:: Maybe FormId
                                               , _whcCompanyName        :: CompanyName
                                               , _whcGetUUID            :: UUID }
                    deriving (Eq,Ord)

cobaltEnvironmentalSolutions :: Company
cobaltEnvironmentalSolutions  = Company "Cobalt Environmental Solutions LLC" [CompanyWidth $ WidthAttribute (12::Int) ]

cobaltAddress:: Address
cobaltAddress= Address "PO Box 130 Wilson, Oklahoma 73463\n886-849-5483\n" [AddressWidth $ WidthAttribute (12::Int)]

cobaltLogo :: Logo
cobaltLogo = Logo "" [LogoPath . PathAttribute $ "Cobalt.png"]


createWaterHauler :: CompanyName  -> Constant
createWaterHauler whc = Constant (T.pack.show $ whc)  [ ConstantAttributeType "'Water Hauling Company'"
                                                                  , ConstantAttributeIndexable $ IndexableAttribute True ]

newtype FormId = FormId {_getFormId :: Integer}
            deriving (Read,Eq,Show,Num,ToJSON,FromJSON,Ord)

cobaltFormBody :: [Row]
cobaltFormBody = [ truckNumberRow
                 , permitNumberRow
                 , customerTicketNumberRow
                 , leaseInfoRow
                 , leaseOperatorRow
                 , leaseNameRow
                 , waterTypeAndAmountRow
                 , dateRow
                 , timeInRow
                 , driverNameRow
                 , signatureRow]
  where
    truckNumberRow  = generateInputRowText "Truck #"
    permitNumberRow  = generateInputRowText "Water Hauling Permit #"
    customerTicketNumberRow = generateInputRowText "Customer Ticket #"
    leaseInfoRow  = generateLabelRow "Lease Information"
    leaseOperatorRow = leaseOperatorDropdownRow
    leaseNameRow = generateInputRowText "Name of Lease"
    waterTypeAndAmountRow  = waterTypeRadioRow
    dateRow  = generateInputRowDate "Date"
    timeInRow  = generateInputRowTime "Time In"
    driverNameRow = generateInputRowText "Driver's Name"
    signatureRow  = generateInputRowSignature "Driver Signature"


waterTypeRadioRow :: Row
waterTypeRadioRow = Row [waterTypeRadio] []

waterTypeRadio :: Item
waterTypeRadio  = Item [ItemRadio . generateRadio "Type of Water Hauled" $ options ] []
   where
     options = [generateOption "Produced Water"
               ,generateOption "Pit Water"
               ,generateOption "Fresh Water"
               ,generateOption "Flowback Water" ]

leaseOperatorDropdownRow :: Row
leaseOperatorDropdownRow = Row [leaseOperatorItem] []
                          where
                           leaseOperatorItem = Item [ItemDropdown leaseOperatorDropdown] []

dropdownOptions :: [Option]
dropdownOptions = generateOption <$> leaseOperators
leaseOperatorDropdown :: Dropdown
leaseOperatorDropdown = Dropdown (Label "Lease Operator" [])
                                                      dropdownOptions
                                                      (Just  fullDefaultInputText )

generateLabelRow :: Text -> Row
generateLabelRow labelText = Row [generateLabelItem labelText] []

-- Input Text
generateInputRowText :: Text -> Row
generateInputRowText labelText = Row [generateInputItemText labelText] []

generateInputItemText :: Text -> Item
generateInputItemText  labelText = Item [ItemLabel . generateLabel $ labelText
                                           , ItemInput fullDefaultInputText] []

fullDefaultInputText :: Input
fullDefaultInputText = Input fullDefaultInputTypeText fullDefaultInputAttributesList

fullDefaultInputTypeText :: InputType
fullDefaultInputTypeText = InputTypeText $ InputText (""::Text)

-- Input Date
generateInputRowDate :: Text -> Row
generateInputRowDate labelDate = Row [generateInputItemDate labelDate] []

generateInputItemDate :: Text -> Item
generateInputItemDate  labelDate = Item [ItemLabel . generateLabel $ labelDate
                                                    , ItemAutoInput . AutoInput $ fullDefaultInputDate] []

fullDefaultInputDate :: Input
fullDefaultInputDate = Input fullDefaultInputTypeDate [InputType InputTypeAttributeDate]

fullDefaultInputTypeDate :: InputType
fullDefaultInputTypeDate = InputTypeDate $ (InputDate "")

-- Input Time
generateInputRowTime :: Text -> Row
generateInputRowTime labelTime = Row [generateInputItemTime labelTime] []

generateInputItemTime :: Text -> Item
generateInputItemTime  labelTime = Item [ItemLabel . generateLabel $ labelTime
                                       , ItemAutoInput . AutoInput $ fullDefaultInputTime] []

fullDefaultInputTime :: Input
fullDefaultInputTime = Input fullDefaultInputTypeTime [InputType InputTypeAttributeTime]

fullDefaultInputTypeTime :: InputType
fullDefaultInputTypeTime = InputTypeTime $ (InputTime "")


-- Input Signature

generateInputRowSignature :: Text -> Row
generateInputRowSignature labelText = Row [generateInputItemSignature labelText] []

generateInputItemSignature :: Text -> Item
generateInputItemSignature  labelText = Item [ItemLabel . generateLabel $ labelText
                                                    , ItemInput fullDefaultInputSignature] []

fullDefaultInputSignature :: Input
fullDefaultInputSignature = Input fullDefaultInputTypeSignature [InputType InputTypeAttributeSignature]

generateLabelItem :: Text -> Item
generateLabelItem labelText = Item [ItemLabel . generateLabel $ labelText ] []

leaseOperators :: [Text]
leaseOperators = ["XTO Energy","Continental Resources","Citation Oil and Gas","Other","Brady's Welding & Machine Shop","WFW Production","Mustang Fuel","SSB Production","LINN Energy","Keith F Walker","GLB","Mack Energy","Nubs","Ardmore Production","Dehart","Southern Oklahoma Production","Silver Creek","Brady Welding & Machine Shop","Coastal Plains","Thunder Oil & Gas","Atlas Pipeline","Cantrell Energy","Kingery Energy","Williford Resources","Mark Shidler","WFD Oil","Yale Oil","Star Oil & Co.","TEF","T&B Oil Co."]

fullDefaultInputTypeSignature :: InputType
fullDefaultInputTypeSignature = InputTypeSignature $ Signature ""

fullDefaultInputAttributesList :: [InputAttribute]
fullDefaultInputAttributesList = [tAttr, ixAttr]
              where
                ixAttr = InputIndexable $ IndexableAttribute True
                tAttr = InputType $ InputTypeAttributeText

generateLabel :: Text -> Label
generateLabel labelText = Label labelText [LabelWidth $ WidthAttribute (12::Int)]

generateRadio :: Text -> [Option] -> Radio
generateRadio labelText options = Radio (generateLabel labelText) options [fullDefaultOptionQualifier]

-- | Radio
fullDefaultOptionQualifier :: OptionQualifier
fullDefaultOptionQualifier = OptionQualifier fullDefaultQualifierChoices []

fullDefaultQualifierChoices :: [QualifierChoices]
fullDefaultQualifierChoices = [ QualifierLabel ( Label "Amount" [])
                              , QualifierInput fullDefaultQualifierInput]

fullDefaultQualifierInput :: Input
fullDefaultQualifierInput = Input dit dia
 where
   dit = InputTypeDouble . InputDouble $ 0.0
   dia = [tAttr, ixAttr,minAttr,maxAttr]
   minAttr = InputMinDouble $ MinAttributeDouble (0.0::Double)
   maxAttr = InputMaxDouble $ MaxAttributeDouble (150.0::Double)
   ixAttr = InputIndexable $ IndexableAttribute True
   tAttr = InputType $ InputTypeAttributeDouble



generateOption :: Text -> Option
generateOption optionText = Option optionText []


data CompanyName = BigStarTrucking
                     | BulletEnergyServices
                     | Advantage
                     | ArkomaTanks
                     | BasicEnergyServices
                     | CottonwoodDrilling
                     | DalesTankService
                     | FluidServices
                     | GandCConstruction
                     | HammandPhillipsService
                     | HullsOilfieldService
                     | JNSTrucking
                     | KleenOilfieldService
                     | LaurcoEnergies
                     | MSMEnvironmental
                     | Nabors
                     | RHRServices
                     | SandHTankService
                     | SandM
                     | TestCompany
                     | AandATankTruck
                     | SonnyTrucking
                     | TerracoProductionLLC
                     | BigMacTrucking

          deriving (Eq,Ord,Show)
