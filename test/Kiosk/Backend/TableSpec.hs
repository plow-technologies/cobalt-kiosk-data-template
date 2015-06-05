{-# LANGUAGE TemplateHaskell #-}
module Kiosk.Backend.TableSpec ( main
                               , spec
                               , getDate
                               , getFormId
                               , getUUID
                               , getTicketId) where

import           Control.Applicative
import           Control.Lens
import           Data.List           (unfoldr)
import           Data.Maybe
import           Data.Time
import qualified Data.UUID           as UUID
import           Generators          (GeneratorType (..),
                                      generateDataTemplateEntry)
import           Kiosk.Backend.Data
import           Plow.Extras.Time
import           Test.Hspec
import           Test.QuickCheck
import           Test.Serial         ()

makeLenses ''DataTemplateEntryKey

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
     describe "Fakey Fae" $
      it "fka fka" $ do
       dtes <- timeCorrectedTemplateEntries
       dtes `shouldBe` dtes





-- | Table Imports

timeCorrectedTemplateEntries :: IO [DataTemplateEntry]
timeCorrectedTemplateEntries = do
                             ct <- getCurrentTime
                             entries <- generateEntries
                             return $ fixDate <$> (zip (generateTimes (startTime ct) 10000) entries)
  where
    generateEntries = generate $ take 50 <$>
                      generateDataTemplateEntry Static
    fixDate :: (Int,DataTemplateEntry)  -> DataTemplateEntry
    fixDate entries = fixFormId.fixUUID . snd $ over (_2.dataTemplateEntryKey.getDate) (const $ fst entries) entries
    fixUUID :: DataTemplateEntry -> DataTemplateEntry
    fixUUID entries = over (dataTemplateEntryKey. getUUID)
                           (const . fromJust .  UUID.fromString $ "a2e3609e-154d-4e60-80e0-c77189098617") entries
    fixFormId entries = over (dataTemplateEntryKey.getFormId) (const 0) entries

generateTimes :: Integral a => UTCTime -> a -> [a]
generateTimes seedTime delta = take 50 $
                               unfoldr
                               (\t -> Just (t, t + delta)) (utcTimeToInt seedTime)

-- Time to start tests at 100ks (1.157 days) ago
startTime :: UTCTime -> UTCTime
startTime ct = addUTCTime (fromIntegral (-1*1000*100)) ct

