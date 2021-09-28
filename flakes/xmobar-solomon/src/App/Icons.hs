{-# LANGUAGE LambdaCase #-}
module App.Icons where

import qualified Web.FontAwesomeType as FA

class Render a where
  render :: a -> String

data Icon =
    HDD
  | BatteryEmpty
  | BatteryQuarter
  | BatteryHalf
  | BatteryThreeQuarters
  | BatteryFull
  | VolumeUp
  | VolumeDown
  | VolumeOff
  | Wifi
  | NoComment
  | Comment
  | Plug
  | Bolt
  deriving Show

instance Render Icon where
  render = \case
    HDD                  -> showIcon FA.FaHddO
    BatteryEmpty         -> showIcon FA.FaBatteryEmpty
    BatteryQuarter       -> showIcon FA.FaBatteryQuarter
    BatteryHalf          -> showIcon FA.FaBatteryHalf
    BatteryThreeQuarters -> showIcon FA.FaBatteryThreeQuarters
    BatteryFull          -> showIcon FA.FaBatteryFull
    VolumeUp             -> showIcon FA.FaVolumeUp
    VolumeDown           -> showIcon FA.FaVolumeDown
    VolumeOff            -> showIcon FA.FaVolumeOff
    Wifi                 -> showIcon FA.FaWifi
    Plug                 -> showIcon FA.FaPlug
    Bolt                 -> showIcon FA.FaBolt
    NoComment            -> wrapIcon ['\62643']
    Comment              -> wrapIcon ['\61557']
    where
      showIcon :: FA.FontAwesome -> String
      showIcon = wrapIcon . pure . FA.fontAwesomeChar

      wrapIcon :: String -> String
      wrapIcon str = mconcat ["<fn=1>", str, "</fn>"]
