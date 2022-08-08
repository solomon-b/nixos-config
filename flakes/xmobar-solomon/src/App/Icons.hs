{-# LANGUAGE LambdaCase #-}
module App.Icons where

import qualified Web.FontAwesomeType as FA

class Render a where
  render :: a -> String

data Icon =
    HDD
  | Battery10
  | Battery20
  | Battery30
  | Battery40
  | Battery50
  | Battery60
  | Battery70
  | Battery80
  | Battery90
  | BatteryEmpty
  | BatteryFull
  | BatteryChargingLow
  | BatteryChargingMed
  | BatteryChargingHigh
  | BatteryChargingEmpty
  | VolumeUp
  | VolumeDown
  | VolumeOff
  | VolumeOn
  | Wifi
  | DoNotDisturb
  deriving Show

iconToChar :: Icon -> Char
iconToChar = \case
  HDD -> '\xF02CA'
  Battery10 -> '\xF007A'
  Battery20 -> '\xF007B'
  Battery30 -> '\xF007C'
  Battery40 -> '\xF007D'
  Battery50 -> '\xF007E'
  Battery60 -> '\xF007F'
  Battery70 -> '\xF0080'
  Battery80 -> '\xF0081'
  Battery90 -> '\xF0082'
  BatteryEmpty -> '\xF008E'
  BatteryFull -> '\xF0079'
  BatteryChargingLow -> '\xF12A4'
  BatteryChargingMed -> '\xF12A5'
  BatteryChargingHigh-> '\xF12A6'
  BatteryChargingEmpty -> '\xF089F'
  VolumeUp -> '\xF075D'
  VolumeDown -> '\xF075E'
  VolumeOff -> '\xF057F'
  VolumeOn -> '\xF057E'
  Wifi -> '\xF05A9'
  DoNotDisturb -> '\xF0376'
    
instance Render Icon where
  render icon = mconcat ["<fn=1>", [iconToChar icon], "</fn>"]
