{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# HLINT ignore "Use <$>" #-}

-- | Fetch and print ACPI power status using Font Awesome icons.
--
-- Current Behavior:
--   1. Fetch AC and all BAT* sources.
--   2. Print AC status.
--   3. Print any batteries whose charge status is not full.
module App.Acpi where

--------------------------------------------------------------------------------

import App.Icons
import Data.Foldable (fold)
import Data.Function ((&))
import Data.List.Extra (trim, isPrefixOf)
import Data.Maybe (mapMaybe)
import Data.Ratio ((%))
import Data.Time.Clock (DiffTime, NominalDiffTime)
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Directory (listDirectory)
import Xmobar (Exec (..))

--------------------------------------------------------------------------------

data Attr = EnergyNowAttr | EnergyFullAttr | StatusAttr

instance Render Attr where
  render EnergyNowAttr  = "charge_now"
  render EnergyFullAttr = "charge_full"
  render StatusAttr     = "status"

--------------------------------------------------------------------------------

newtype Battery         = Battery Int
newtype EnergyNow       = EnergyNow       { getEnergyNow       :: Integer }
newtype EnergyFull      = EnergyFull      { getEnergyFull      :: Integer }
newtype EnergyRemaining = EnergyRemaining { getEnergyRemaining :: Integer }
data ChargeStatus       = Charging | Discharging | Full deriving stock (Show, Eq)

--------------------------------------------------------------------------------

-- | Derived Battery Properties
newtype EnergyPercent = EnergyPercent { getEnergyPercent :: Rational }

instance Render EnergyPercent where
  render energy = show (fromEnergyPercent energy) <> "%"

--------------------------------------------------------------------------------

data BatteryStatus = BatteryStatus
  { _battery :: Battery
  , _energyPercent :: EnergyPercent
  , _chargeStatus :: ChargeStatus
  }

instance Render BatteryStatus where
  render (BatteryStatus bat energy status) =
    let icon =  render . iconForBattery status $ energy
    in icon <> render energy

--------------------------------------------------------------------------------

diffToNominal :: DiffTime -> NominalDiffTime
diffToNominal = fromRational . toRational

formatRemainingTime :: DiffTime -> String
formatRemainingTime = formatTime defaultTimeLocale "%H:%M" . posixSecondsToUTCTime . diffToNominal

fromEnergyPercent :: EnergyPercent -> Integer
fromEnergyPercent = round @Double . (* 100) . realToFrac . getEnergyPercent

iconForBattery :: ChargeStatus -> EnergyPercent -> Icon
iconForBattery Discharging (EnergyPercent rat)
  | rat <= 1 % 10 = BatteryEmpty
  | rat <= 2 % 10 = Battery10
  | rat <= 3 % 10 = Battery20
  | rat <= 4 % 10 = Battery30
  | rat <= 5 % 10 = Battery40
  | rat <= 6 % 10 = Battery50
  | rat <= 7 % 10 = Battery60
  | rat <= 8 % 10 = Battery70
  | rat <= 9 % 10 = Battery80
  | rat <= 10 % 10 = Battery90
  | otherwise = BatteryFull
iconForBattery Charging (EnergyPercent rat)
  | rat <= 1 % 4 = BatteryChargingEmpty
  | rat <= 2 % 4 = BatteryChargingLow
  | rat <= 4 % 4 = BatteryChargingMed
  | otherwise = BatteryChargingHigh
iconForBattery Full _ = BatteryChargingHigh

toChargeStatus :: String -> ChargeStatus
toChargeStatus str =
  case str of
      "Charging"    -> Charging
      "Discharging" -> Discharging
      _             -> Full

toEnergyRemaining :: EnergyNow -> EnergyFull -> EnergyRemaining
toEnergyRemaining (EnergyNow now) (EnergyFull full) = EnergyRemaining $ full - now

toEnergyPercent :: EnergyNow -> EnergyFull -> EnergyPercent
toEnergyPercent (EnergyNow now) (EnergyFull full) = EnergyPercent $ now % full

--------------------------------------------------------------------------------

-- | Lookup the 'BatteryStatus' for a particular battery. 
getAcpiBat :: Battery -> IO BatteryStatus
getAcpiBat bat@(Battery i) = do
  let sysFsPath = "/sys/class/power_supply/" <> "BAT" <> show i <> "/"
  energyNow  <- EnergyNow  . read . trim <$> readFile (sysFsPath <> render EnergyNowAttr)
  energyFull <- EnergyFull . read . trim <$> readFile (sysFsPath <> render EnergyFullAttr)
  status     <- toChargeStatus    . trim <$> readFile (sysFsPath <> render StatusAttr)
  let energyPercent   = toEnergyPercent   energyNow energyFull
  let energyRemaining = toEnergyRemaining energyNow energyFull

  pure $ BatteryStatus bat energyPercent status

-- | Parse Battery battery identifiers.
parseBattery :: String -> Maybe Battery
parseBattery = \case
   x | isPrefixOf "BAT" x -> Just $ Battery $ read $ drop 3 x
   _ -> Nothing

-- | Lookup and parse battery names.
findBatteries :: IO [Battery]
findBatteries =
  let sysFsPath = "/sys/class/power_supply"
  in fmap (mapMaybe parseBattery) $ listDirectory sysFsPath

-- | Main entrypoint for application.
runAcpi :: IO String
runAcpi = do
  batteries <- findBatteries
  bStates <- traverse getAcpiBat batteries
  pure $ foldMap render bStates

--------------------------------------------------------------------------------

newtype Acpi = Acpi String
  deriving stock (Show, Read)

instance Exec Acpi where
  run :: Acpi -> IO String
  run _ = runAcpi

  alias :: Acpi -> String
  alias (Acpi a) = a
