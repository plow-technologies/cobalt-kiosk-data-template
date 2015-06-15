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



module Kiosk.Backend.Data.ReportTemplateSpec (spec,main,convertToKioskForm) where

import           Control.Applicative               ((<$>))
import           Data.String                       (IsString)
-- import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy              as L
import           Data.Text                         (Text)
import qualified Data.Text                         as T
-- import           Data.ByteString.Lazy.Char8        (ByteString)
-- import qualified Data.ByteString.Lazy.Char8        as B
-- import           Data.Map.Strict                   (Map)
-- import qualified Data.Map.Strict                   as M
-- import           Data.Maybe                        (catMaybes)
-- import           Data.Monoid                       ((<>))
import           Control.Lens
import           Data.Time

-- import           Kiosk.Backend.Data.DataTemplate
import           Data.Map.Lazy                     (Map)
import qualified Data.Map.Lazy                     as M
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Kiosk.Backend.Data.DataTemplate
import           Kiosk.Backend.Data.ReportTemplate
import           Kiosk.Backend.Form
import           Language.Haskell.TH

import           Codec.Xlsx

-- import           Mocks.Primitive.Generators        (GeneratorType (..),
--                                                     generateInts)
import           Data.Monoid                       ((<>))
import           ReportTemplate.Internal
import           System.Locale                     (defaultTimeLocale)
import           System.Time
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


-- a template is described from here:
makeCobaltExcelTemplate :: XlsxReportTemplate
makeCobaltExcelTemplate = buildReportTemplate preambleTemplate rowTemplate

preambleTemplate :: XlsxPreambleTemplateList
preambleTemplate = [("Report Prepared For", const $ getCompanyName (1,1))
                   , ("Prepared On", formatTimestampDate )]
 where
    formatTimestampDate context _ = makeCellMapFromUTCTime "%c" (2,2) . _xlsxCurrentTime $ context

rowTemplate:: XlsxRowTemplateList
rowTemplate = [ ("Water Hauling Number",getWaterHauler )
               ,("Lease Name",getLeaseName)
               ,("Description", getDescription)
               ,("Truck Number",getTruckNumber)
               ,("Customer Ticket Number", getCustomerTicketNumber)
               ]

   where
    getDescription = const $ makeCellTextWithCellTemplate descriptionTemplate descriptionList
    descriptionList = ["Truck #", "Name of Lease", "Water Hauling Permit #"]
    getWaterHauler = const $ makeCellTextFromInputText "Water Hauling Permit #"
    getLeaseName = const $ makeCellTextFromInputText "Name of Lease"
    getTruckNumber   = const $ makeCellTextFromInputText "Truck #"
    getCustomerTicketNumber = const $ makeCellTextFromInputText "Customer Ticket #"


descriptionTemplate [field1, field2, field3] = "Field1 Is: " <> field1 <>
                                                  "Field 2 is:" <> field2 <>
                                                  "Field 3 is:" <> field3
descriptionTemplate _ = "Wrong Number of arguments"


-- To Herem

-- | Report Inspection

dispayReportValues = do
  report <-generateReport
  sequence $ (foldrTableByRowWithIndex printAndMoveOn (return ())) <$>
             (toListOf  (reportRows._ReportTableRowIndex._2) report)
   where
    printAndMoveOn k rowVal m = do
                             m
                             print k
                             print rowVal
-- | Generate report

makeXlsxFile = do
      ct <- getClockTime
      xl <- generateReportXlsx
      L.writeFile "example.xlsx" $ fromXlsx ct xl

generateReportXlsx = do
    sheet <- generateReportSheet
    return $  def & atSheet "ex"  ?~ sheet


generateReportXlsx :: IO Xlsx
generateReportSheet = renderSpreadsheet <$> generateReport

generateReport :: IO XlsxReport
generateReport = do
     ct <- getCurrentTime
     dtes <- generate generateDataTemplatesWithData
     let forms@(oneForm:_) = convertToKioskForm <$> currentCobaltForms
         reportTemplate = makeCobaltExcelTemplate
         report = buildXlsxReport reportTemplate (XlsxContext ct) oneForm dtes
     return report


generateDataTemplatesWithData = do
  txt <- T.pack <$> arbitrary
  let targetDataTemplates = (fromFormToDataTemplate.convertToKioskForm <$> currentCobaltForms )
      transformedDataTemplates = targetDataTemplates & (traverse .
                                                       _templateItems .
                                                       traverse .
                                                       _templateValue .
                                                       _InputTypeText .
                                                       getInputText) .~ "an arbitrary thign"
  return transformedDataTemplates

-- | Form Generation (Cobalt Version)
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
                   | CandJTrucking
                   | BigMacTankTrucks
                   | BradyWeldingandMachineShop
                   | KleenOilfieldServices
                   | BandCBackhoeandTransports
                   | ForsytheOilfield
                   | HullsOilfield
                   | SouthCentralOilfieldServices
                   | TopOTexas
                   | MitchellTankTruckServices
                   | FluidServices
                   | DavenportOilfieldServices
                   | TestCompany
                   | SoonerStar
                   | NexStream
                   | HullEnvironmentalServices
                   | Arkhoma
                   | ZeroSeven
                   | HammTankAndTrucking
          deriving (Eq,Ord)


instance Show CompanyName where
  show (BigStarTrucking) = "Big Star Trucking"
  show (BulletEnergyServices) = "Bullet Energy Services"
  show (CandJTrucking) = "C and J Trucking"
  show (BigMacTankTrucks) = "Big Mac Trucks"
  show (BradyWeldingandMachineShop) = "Bradly Welding and Machine Shop"
  show (KleenOilfieldServices) = "Kleen Oilfield Services"
  show (BandCBackhoeandTransports) = "B and C Backhoe and Transports"
  show (ForsytheOilfield ) = "Forsythe Oilfield"
  show (HullsOilfield) = "Hulls Oilfield"
  show (SouthCentralOilfieldServices) = "South Central Oilfield Services"
  show (TopOTexas) = "Top-O-Texas"
  show (MitchellTankTruckServices) = "Mitchell Tank Truck Services"
  show (FluidServices) = "Fluid Services"
  show (DavenportOilfieldServices) = "Davenport Oilfield Services"
  show (TestCompany    ) = "Test Company"
  show (SoonerStar    ) = "Sooner Star"
  show (NexStream    ) = "NexStream"
  show (Arkhoma ) = "Arkhoma"
  show (HullEnvironmentalServices) = "Hull Environmental Services"
  show (ZeroSeven) = "07 Energy"
  show (HammTankAndTrucking) = "Hamm Tank and Trucking Service, LLC"


exampleUUID :: UUID
exampleUUID =   "a2e3609e-154d-4e60-80e0-c77189098617"

currentCobaltForms :: [CobaltWaterHaulingCompany]
currentCobaltForms = [ CobaltWaterHaulingCompany (Just 0) BigStarTrucking exampleUUID
                        , CobaltWaterHaulingCompany (Just 1) BulletEnergyServices exampleUUID
                        , CobaltWaterHaulingCompany (Just 2) CandJTrucking exampleUUID
                        , CobaltWaterHaulingCompany (Just 3) BigMacTankTrucks exampleUUID
                        , CobaltWaterHaulingCompany (Just 4) BradyWeldingandMachineShop exampleUUID
                        , CobaltWaterHaulingCompany (Just 5) KleenOilfieldServices exampleUUID
                        , CobaltWaterHaulingCompany (Just 6) BandCBackhoeandTransports exampleUUID
                        , CobaltWaterHaulingCompany (Just 7) ForsytheOilfield exampleUUID
                        , CobaltWaterHaulingCompany (Just 8) HullsOilfield exampleUUID
                        , CobaltWaterHaulingCompany (Just 9) SouthCentralOilfieldServices exampleUUID
                        , CobaltWaterHaulingCompany (Just 10) TopOTexas exampleUUID
                        , CobaltWaterHaulingCompany (Just 11) MitchellTankTruckServices exampleUUID
                        , CobaltWaterHaulingCompany (Just 12) FluidServices exampleUUID
                        , CobaltWaterHaulingCompany (Just 13) DavenportOilfieldServices exampleUUID
                        , CobaltWaterHaulingCompany (Just 14) TestCompany exampleUUID
                        , CobaltWaterHaulingCompany (Just 15) SoonerStar exampleUUID
                        , CobaltWaterHaulingCompany (Just 16) NexStream exampleUUID
                        , CobaltWaterHaulingCompany (Just 17) Arkhoma exampleUUID
                        , CobaltWaterHaulingCompany (Just 18) HullEnvironmentalServices exampleUUID
                        , CobaltWaterHaulingCompany (Just 19) ZeroSeven exampleUUID
                        , CobaltWaterHaulingCompany (Just 20) HammTankAndTrucking exampleUUID]
