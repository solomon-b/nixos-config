module App (config) where

import           App.Acpi
import           App.DunstStatus
import           App.Icons
import           Xmobar

--------------
--- Colors ---
--------------

--- Sanity Inc 80s
background = "#2d2d2d"
altBackground = "#333333"
currentLine = "#393939"
selection = "#515151"
foreground = "#cccccc"
comment = "#999999"
red = "#f2777a"
orange = "#f99157"
yellow = "#ffcc66"
green = "#99cc99"
aqua = "#66cccc"
blue = "#6699cc"
purple = "#cc99cc"

------------
--- Main --- 
------------

config :: Config
config = defaultConfig
  { font = "xft:Bitstream Vera Sans Mono:size=11:antialias=true"
  , additionalFonts = ["xft:Font Awesome 5 Free Solid:style=Solid:size=11"]
  , allDesktops  = False
  , pickBroadest = True
  , bgColor      = background
  , fgColor      = foreground
  , alpha        = 255
  , position     = TopW L 100
  , commands = [
        Run StdinReader
      --, Run $ DiskU [("/", render HDD <> " <used>/<size>")] ["-L","20","-H","50","-m","1","-p","3"] 20
      , Run $ DiskU [("/dev/sda1", render HDD <> " <used>/<size>")] ["-L","20","-H","50","-m","1","-p","3"] 20
      , Run $ Volume "default" "Master"
          [ "-t", "<status> <volume>%" , "--", "-O", render VolumeUp, "-o", render VolumeOff, "-c", red, "-C", foreground ] 1
      , Run $ Date ("<fc=" <> yellow <> ">%a %b %_d %Y %I:%M %p</fc>") "date" 9
      , Run $ Wireless "wlp4s0" [ "--template" , render Wifi <> " <essid>" ] 10
      , Run $ Acpi "battery"
      , Run $ DunstStatus "dunstStatus"
      ]
  , sepChar = "%"
  , alignSep = "}{"
  , template = "%StdinReader% }{ %disku% %default:Master% %battery% %wlp4s0wi% %dunstStatus% %date%          "
  }
