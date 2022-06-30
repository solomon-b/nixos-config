{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
module App.Acpi where

import App.Icons

import Data.List.Extra (trim)
import Data.Ratio
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Clock.POSIX
import Xmobar (Exec(..))

---------------
---- Types ----
---------------

newtype Battery = Battery Int
data Attr = EnergyNowAttr | EnergyFullAttr | StatusAttr | PowerNowAttr

-- | ACPI Battery Properties
newtype EnergyNow       = EnergyNow       { getEnergyNow       :: Integer }
newtype EnergyFull      = EnergyFull      { getEnergyFull      :: Integer }
newtype EnergyRemaining = EnergyRemaining { getEnergyRemaining :: Integer }
newtype PowerNow        = PowerNow        { getPowerNow        :: Integer }
data ChargeStatus       = Charging | Discharging | Full deriving stock Eq

-- | Derived Battery Properties
newtype TimeRemaining = TimeRemaining { getTime :: DiffTime }
newtype EnergyPercent = EnergyPercent { getEnergyPercent :: Rational }

-- | Power Sources
data AcStatus = Connected | Disconnected deriving stock Eq
data BatteryStatus = BatteryStatus
  { _battery :: Battery
  , _energyPercent :: EnergyPercent
  , _chargeStatus :: ChargeStatus
  , _timeRemaining :: TimeRemaining
  }

---------------------
---- TypeClasses ----
---------------------

instance Render Battery where
  render (Battery n) = "BAT" <> show n

instance Render Attr where
  render EnergyNowAttr  = "energy_now"
  render EnergyFullAttr = "energy_full"
  render StatusAttr     = "status"
  render PowerNowAttr   = "power_now"

instance Render EnergyPercent where
  render energy =
    let icon    =  render . iconForBattery $ energy
        percent =  show (fromEnergyPercent energy) <> "%"
    in icon <> " " <> percent

instance Render TimeRemaining where
  render = formatRemainingTime . getTime

instance Render AcStatus where
  render Disconnected = "AC"
  render Connected    = render Plug <> " AC"

instance Render BatteryStatus where
  render (BatteryStatus bat energy status timeRemaining) =
    let chargingIcon   = render Bolt
        bat'           = render bat
        energy'        = render energy
        timeRemaining' = render timeRemaining
        space          = " "
    in if status == Charging
       then bat' <> space <> energy' <> space <> chargingIcon
       else if status == Discharging
            then bat' <> space <> render energy <> space <> timeRemaining'
            else bat' <> space <> render energy

------------------------
---- Pure Functions ----
------------------------

diffToNominal :: DiffTime -> NominalDiffTime
diffToNominal = fromRational . toRational

formatRemainingTime :: DiffTime -> String
formatRemainingTime = formatTime defaultTimeLocale "%H:%M" . posixSecondsToUTCTime . diffToNominal

fromEnergyPercent :: EnergyPercent -> Integer
fromEnergyPercent = round @Double . (* 100) . realToFrac . getEnergyPercent

iconForBattery :: EnergyPercent -> Icon
iconForBattery (EnergyPercent rat)
  | rat <= 1 % 8 = BatteryEmpty
  | rat <= 1 % 4 = BatteryQuarter
  | rat <= 1 % 2 = BatteryHalf
  | rat <= 3 % 4 = BatteryThreeQuarters
  | otherwise = BatteryFull


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

toTimeRemaining :: EnergyRemaining -> PowerNow -> TimeRemaining
toTimeRemaining remain pow =
  let rem' = fromInteger . getEnergyRemaining $ remain
      pow' = fromInteger . getPowerNow        $ pow
  in TimeRemaining . secondsToDiffTime . round @Double . (* 3600) $ rem' / pow'

-----------------------------
---- Effectful Functions ----
-----------------------------

getAcpiAc :: IO AcStatus
getAcpiAc = do
  let sysFsPath = "/sys/class/power_supply/ACAD/online"
  ac <- (== "1") . trim <$> readFile sysFsPath
  if ac then return Connected else return Disconnected

getAcpiBat :: Battery -> IO BatteryStatus
getAcpiBat bat = do
  let sysFsPath = "/sys/class/power_supply/" ++ render bat ++ "/"
  energyNow  <- EnergyNow  . read . trim <$> readFile (sysFsPath ++ render EnergyNowAttr)
  energyFull <- EnergyFull . read . trim <$> readFile (sysFsPath ++ render EnergyFullAttr)
  status     <- toChargeStatus    . trim <$> readFile (sysFsPath ++ render StatusAttr)
  power      <- PowerNow   . read . trim <$> readFile (sysFsPath ++ render PowerNowAttr)
  let energyPercent   = toEnergyPercent   energyNow energyFull
  let energyRemaining = toEnergyRemaining energyNow energyFull
  let timeRemaining   = toTimeRemaining   energyRemaining power

  pure $ BatteryStatus bat energyPercent status timeRemaining

printStatus :: AcStatus -> BatteryStatus -> BatteryStatus -> String
printStatus ac bat0@(BatteryStatus _ _ chargeStatus _) bat1@(BatteryStatus _ _ chargeStatus' _)
  | chargeStatus  /= Full = render bat0
  | chargeStatus' /= Full = render bat1
  | otherwise             = render ac

runAcpi :: IO String
runAcpi = do
  ac   <- getAcpiAc
  bat0 <- getAcpiBat (Battery 0)
  bat1 <- getAcpiBat (Battery 1)
  pure $ printStatus ac bat0 bat1

----------------
---- Plugin ----
----------------

newtype Acpi = Acpi String
  deriving stock (Show, Read)

instance Exec Acpi where
  run :: Acpi -> IO String
  run _ = runAcpi

  alias :: Acpi -> String
  alias (Acpi a) = a
