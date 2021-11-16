module App (config) where

import           App.Acpi
import           App.DunstStatus
import           App.Icons
import           Xmobar

config :: Config
config = defaultConfig
  { font = "xft:Bitstream Vera Sans Mono:size=11:bold:antialias=true"
  , additionalFonts = ["xft:Font Awesome 5 Free Solid:style=Solid:size=11"]
  , allDesktops  = False
  , pickBroadest = True
  , bgColor      = "#2d2d2d"
  , fgColor      = "#515151"
  , alpha        = 175
  , position     = TopW L 100 --Static { xpos = 0, ypos = 0, width = 1920, height = 23 } --TopW L 95
  , commands = [
        Run StdinReader
      --, Run $ DiskU [("/", render HDD <> " <used>/<size>")] ["-L","20","-H","50","-m","1","-p","3"] 20
      , Run $ DiskU [("/dev/sda1", render HDD <> " <used>/<size>")] ["-L","20","-H","50","-m","1","-p","3"] 20
      , Run $ Volume "default" "Master"
          [ "-t", "<status> <volume>%" , "--", "-O", render VolumeUp, "-o", render VolumeOff, "-C", "#585858" ] 1
      , Run $ Date "<fc=#ffcc66>%a %b %_d %Y %I:%M %p</fc>" "date" 9
      , Run $ Wireless "wlp4s0" [ "--template" , render Wifi <> " <essid>" ] 10
      , Run $ Acpi "battery"
      , Run $ DunstStatus "dunstStatus"
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%StdinReader% }{ %disku% %default:Master% %battery% %wlp4s0wi% %dunstStatus% %date%          "
  }
