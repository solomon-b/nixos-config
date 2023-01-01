module App (config) where

--------------------------------------------------------------------------------

import App.Acpi
import App.DunstStatus
import App.Icons
import Xmobar

--------------------------------------------------------------------------------
-- Colors

--- Sanity Inc 80s
background :: String
background = "#2d2d2d"

altBackground :: String
altBackground = "#333333"

currentLine :: String
currentLine = "#393939"

selection :: String
selection = "#515151"

foreground :: String
foreground = "#cccccc"

comment :: String
comment = "#999999"

red :: String
red = "#f2777a"

orange :: String
orange = "#f99157"

yellow :: String
yellow = "#ffcc66"

green :: String
green = "#99cc99"

aqua :: String
aqua = "#66cccc"

blue :: String
blue = "#6699cc"

purple :: String
purple = "#cc99cc"

--------------------------------------------------------------------------------
-- Main

config :: Config
config = defaultConfig
  { font = "xft:Bitstream Vera Sans Mono:size=11:antialias=true"
  , additionalFonts = ["xft: Material Design Icons:style=Regular"]
  , allDesktops  = False
  , pickBroadest = True
  , bgColor      = background
  , fgColor      = foreground
  , alpha        = 255
  , position     = TopW L 100
  , commands = [
        Run XMonadLog
      --, Run $ DiskU [("/", render HDD <> " <used>/<size>")] ["-L","20","-H","50","-m","1","-p","3"] 20
      -- , Run $ DiskU [("/dev/sda1", render HDD <> " <used>/<size>")] ["-L","20","-H","50","-m","1","-p","3"] 20
      , Run $ Volume "default" "Master"
        [ "-t", "<status><volume>%" , "--", "-O", render VolumeOn, "-o", render VolumeOff, "-c", red, "-C", foreground ] 10
      , Run $ Acpi "battery"
      , Run $ Wireless "wlp170s0" ["--template", render Wifi <> "<essid>"] 10
      , Run $ DunstStatus "dunstStatus"
      , Run $ Date ("<fc=" <> yellow <> ">%a %b %_d %Y %I:%M %p</fc>") "date" 9
      ]
  , sepChar = "%"
  , alignSep = "}{"
  --, template = "%StdinReader% }{ %disku% %default:Master% %battery% %wlp4s0wi% %dunstStatus% %date%          "
  , template = "%XMonadLog% }{ %dunstStatus% %default:Master% %battery% %wlp170s0wi% %date%      "
  }
