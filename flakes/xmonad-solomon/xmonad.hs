{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
--import Xmonad.Hooks.StatusBar
--import Xmonad.Hooks.StatusBar.PP

import XMonad.Hooks.TaffybarPagerHints
import XMonad.Util.EZConfig
import XMonad.Util.Run (runInTerm, spawnPipe)

import XMonad.Layout.Gaps
import XMonad.Layout.Hidden
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.NoBorders
import XMonad.Layout.PerScreen

import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation

import XMonad.Actions.Commands (defaultCommands)
import XMonad.Actions.CopyWindow
import XMonad.Actions.Navigation2D
import XMonad.Actions.Promote

import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell

import XMonad.Core (recompile)
import XMonad.Operations (restart)
import XMonad.StackSet qualified as W
import Data.Maybe

import System.IO
import System.Exit
import System.Process
import GHC.IO.Handle

import Control.Exception (SomeException, try)
import Control.Monad
import Control.Monad.IO.Class

import Data.Foldable
import Data.Map qualified as M
import Data.Monoid (All(..))
import Data.Char (toLower)
import Data.List (isInfixOf, intersperse, nub)
import Data.Function (on)
import GHC.IO (unsafeInterleaveIO)

--------------------------------------------------------------------------------
-- Theme

background = "#2d2d2d"
altBackground = "#333333"
currentLine = "#393939"
selection = "#515151"
foreground = "#cccccc"
comment = "#999999"

red    = "#f2777a"
orange = "#f99157"
yellow = "#ffcc66"
green  = "#99cc99"
aqua   = "#66cccc"
blue   = "#6699cc"
purple = "#cc99cc"

active       = red
activeWarn   = blue
inactive     = orange
focusColor   = red
unfocusColor = orange

myFont = "xft:Meslo LG M:style=Regular:size=12"

myNormalBorderColor = blue
myFocusedBorderColor = red

myTabTheme = def
  { fontName            = myFont
  , activeColor         = altBackground
  , inactiveColor       = background
  , activeBorderColor   = altBackground
  , inactiveBorderColor = background
  , activeTextColor     = foreground
  , inactiveTextColor   = comment
  }

--------------------------------------------------------------------------------
-- Layouts

gap      = 4
topbar   = 10
myBorder = 1
prompt   = 20
status   = 20

mySpacing :: l a -> ModifiedLayout Spacing l a
mySpacing = spacing gap

myGaps :: l a -> ModifiedLayout Gaps l a
myGaps = gaps [(U, gap),(D, gap),(L, gap),(R, gap)]

trimNamed :: Int -> String -> l a -> ModifiedLayout Rename l a
trimNamed w n = renamed [CutWordsLeft w, PrependWords n]

suffixed :: String -> l a ->  ModifiedLayout Rename l a
suffixed n = renamed [AppendWords n]

trimSuffixed :: Int -> String -> l a -> ModifiedLayout Rename l a
trimSuffixed w n = renamed [CutWordsRight w, AppendWords n]

--------------
--- Layout ---
--------------

data TABBED = TABBED
  deriving (Show, Read, Eq, Typeable)

--instance Transformer MIRROR Window where
--    transform _ x k = k (Mirror x) (\(Mirror x') -> x')

myLayoutHook = avoidStruts $ mirrorToggle $ fullScreenToggle $ flex ||| tabs

data FocusedOnly = FocusedOnly
  deriving (Show, Read)

instance SetsAmbiguous FocusedOnly where
  hiddens :: FocusedOnly -> WindowSet -> Rectangle -> Maybe (W.Stack Window) -> [(Window, Rectangle)] -> [Window]
  hiddens _ wset lr mst wrs =
    case W.peek wset of
      Nothing -> fmap fst wrs
      Just focused -> filter (/= focused) $ fmap fst wrs

fullScreenToggle = mkToggle $ single FULL
mirrorToggle = mkToggle $ single MIRROR

tabs = named "Tabs" . noBorders $ addTabs shrinkText myTabTheme Simplest
flex =
    trimNamed 5 "Flex"
  . noBorders
  . windowNavigation
  . addTabs shrinkText myTabTheme
  . subLayout [] Simplest
  --  $ standardLayouts
  $ ifWider 1920 wideLayouts standardLayout
  where
      wideThreeCol = suffixed "Wide 3Col" (ThreeColMid 1 (1/20) (1/2))
      wideLayouts  = mySpacing . myGaps $ wideThreeCol ||| standardLayout
      standardLayout = mySpacing . myGaps . named "Std 2/3" $
          ResizableTall 1 (1/20) (2/3) [] --  ||| Mirror (ResizableTall 1 (1/20) (2/3) [])

myBrowser  = "/usr/bin/firefox"
myTerminal = "termonad"
myLauncher = XMonad.Prompt.Shell.shellPrompt promptConfig

myWorkspaces = ["1:term","2:web", "3:slack"] ++ map show [4..9]

myManageHook = composeAll
  [ className =? "Firefox" --> doShift "2:web"
  , className =? "Slack"   --> doShift "3:slack"
  , className =? "trayer"  --> doIgnore
  , manageDocks
  ]

--------------------------------------------------------------------------------
-- Prompts

promptConfig :: XPConfig
promptConfig = def
  { position          = Top
  , height            = 20
  , font              = myFont
  , bgColor           = background
  , fgColor           = orange
  , fgHLight          = "#d33682"
  , bgHLight          = "#073642"
  , promptBorderWidth = 0
  , maxComplRows      = Just 12
  , alwaysHighlight   = False
  , promptKeymap      = emacsLikeXPKeymap
  , searchPredicate   = isInfixOf `on` map toLower
  }

-- | X Session logout and system shutdown/reboot prompt
exitPrompt :: X ()
exitPrompt = xmonadPromptCT "Exit" commands promptConfig
  where
    commands =
      [ ("1: Logout",   io exitSuccess)
      , ("2: Shutdown", spawn "systemctl poweroff")
      , ("3: Reboot",   spawn "systemctl reboot")
      ]

-- | Kill the focused window
closeWindowPrompt :: X ()
closeWindowPrompt = confirmPrompt promptConfig "Close Window" kill1

-- | Screenshot prompt
scrotPrompt :: X ()
scrotPrompt = xmonadPromptCT "Screenshot Options" commands promptConfig
  where
    commands = [ ("1: Capture Screen", spawn "scrot")
               , ("2: Capture Selection", spawn "scrot -s")
               , ("3: Capture All Screens", spawn "scrot -m")
               , ("4: Capture with 3 second countdown", spawn "scrot -d 3 -c")
               ]

-- | Given a 'Handle', return a list of all lines in that 'Handle'
hGetLines :: Handle -> IO [String]
hGetLines handle = unsafeInterleaveIO $ do
  isEof <- hIsEOF handle
  if isEof
    then pure []
    else do
      line <- hGetLine handle
      lines <- hGetLines handle
      pure $ line : lines

data SystemUnit = SystemUnit
  { _unit :: String
  , _load :: String
  , _active :: String
  , _sub :: String
  , _description :: String
  } deriving Show

mkSystemUnit :: [String] -> Maybe SystemUnit
mkSystemUnit [a,b,c,d,e] = Just $ SystemUnit a b c d e
mkSystemUnit _ = Nothing

-- | Fetch systemd user units.
--
-- TODO:
fetchUnits :: IO [SystemUnit]
fetchUnits = do
  let p = proc "systemctl" ["--user"]
  (_, hout, _, _) <- createProcess p { std_out = CreatePipe }
  case hout of
    Just hout' -> do
      res <- words <$> hGetLine hout'
      res1 <- mapMaybe (mkSystemUnit . words) <$> hGetLines hout'
      print res
      pure res1
    _ -> pure []

--------------------------------------------------------------------------------
-- Keybindings

workSpaceNav :: XConfig a -> [(String, X ())]
workSpaceNav c = do
  (i, j) <- zip (map show [1..9]) $ XMonad.workspaces c
  (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift)]
  return (m++i, windows $ f j)

myKeys c = mkKeymap c $
  -- System
  [ ("M-q",                     restart)
  , ("M-S-q",                   exitPrompt)
  , ("M-<Backspace>",           closeWindowPrompt)
  , ("M-S-<Backspace>",         withUnfocused killWindow)
  , ("<XF86AudioMute>",         toggleMute)
  , ("<XF86AudioRaiseVolume>",  volumeUp)
  , ("<XF86AudioLowerVolume>",  volumeDown)
  , ("<XF86MonBrightnessUp>",   spawn "brightnessctl set 5%+")
  , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 5%-")
  , ("M-m",                     toggleDunst >> toggleMute)
  , ("M-<Print>",               scrotPrompt)
 -- , ("M-n", runInTerm "" "htop")
  , ("C-<Space>", dunstClose)
  , ("C-S-<Space>", dunstCloseAll)
  ] <>

  -- Navigate between windows
  [ ("M-j",       windowGo D False)
  , ("M-k",       windowGo U False)
  , ("M-h",       windowGo L False)
  , ("M-l",       windowGo R False)
  -- Navigate between tabs
  , ("M-;",       windows W.focusUp)
  , ("M-'",       windows W.focusDown)
  -- Shift tabs
  , ("M-S-;",     windows W.swapUp)
  , ("M-S-'",     windows W.swapDown)
  -- Swap adjacent windows
  , ("M-S-j",     windowSwap D False)
  , ("M-S-k",     windowSwap U False)
  , ("M-S-h",     windowSwap L False)
  , ("M-S-l",     windowSwap R False)
  -- Shrink/Expand windows
  , ("M-[",       sendMessage Shrink)
  , ("M-]",       sendMessage Expand)
  , ("M-<Space>", sendMessage NextLayout)
  , ("M-r",       sendMessage $ Toggle MIRROR)
  , ("M-C-<Space>", toSubl NextLayout)
  -- Float/Sink floated window
  , ("M-t", withFocused toggleFloat >> killAllOtherCopies)
  , ("M-C-t",     withFocused toggleSticky)
  -- Full Screen a window
  , ("M-<F11>",   sendMessage $ Toggle FULL)
  -- Promote window to master
  , ("M-b",       promote)
  -- "merge with sublayout"
  , ("M-C-h",     sendMessage . pullGroup $ L)
  , ("M-C-l",     sendMessage . pullGroup $ R)
  , ("M-C-j",     sendMessage . pullGroup $ D)
  , ("M-C-k",     sendMessage . pullGroup $ U)
  -- Unmerge a window
  , ("M-g",       withFocused (sendMessage . UnMerge))
  ] <> workSpaceNav c <>
  [ ("M-<Return>", spawn myTerminal)     -- Launch Terminal
  , ("M-\\",       spawn myBrowser)      -- Launch Browser
  , ("M-p",        myLauncher)     -- Launch DMenu
  ]
  where
    toggleDunst   = spawn "dunstctl set-paused toggle"
    toggleMute    = spawn "amixer sset 'Master' toggle"
    dunstClose    = spawn "dunstctl close"
    dunstCloseAll = spawn "dunstctl close-all"
    volumeUp      = spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"
    volumeDown    = spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"
    restart       = do
      spawn "pkill trayer"
      spawn "xmonad --restart"
    toggleSticky w = windows $ \s ->
      if M.member w (W.floating s)
      then copyToAll s
      else s
    toggleFloat w  = windows $ \s ->
      if M.member w (W.floating s)
      then W.sink w s
      else W.float w (W.RationalRect (1/6) (1/6) (2/3) (2/3)) s

myNav2DConf = def
  { defaultTiledNavigation = centerNavigation
  , floatNavigation        = centerNavigation
  , screenNavigation       = lineNavigation
  , layoutNavigation       = pure ("Full", centerNavigation)
  , unmappedWindowRect     = pure ("Full", singleWindowRect)
  }

myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
  [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster) -- Set window to float and move by dragging
  , ((modm .|. controlMask, button1), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster) -- Set window to float and resize by dragging
  ]

--------------------------------------------------------------------------------
-- Main

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe path =
  fmap (either (const Nothing) Just) $ try @SomeException $ readFile path

myStartupHook :: X ()
myStartupHook = do
  commands <- liftIO $ readFileMaybe "/home/solomon/.startup" 
  traverse_ (traverse_ spawn) $ fmap lines commands

restartEventHook :: Event -> X All
restartEventHook = \case
  ClientMessageEvent {ev_message_type} -> do
    atom <- getAtom "XMONAD_RESTART"
    when (ev_message_type == atom) $ restart "xmonad-solomon" True
    pure $ All True
  _ -> pure $ All True

myLogHook :: Handle -> X ()
myLogHook xmproc = dynamicLogWithPP xmobarPP
  { ppCurrent         = xmobarColor yellow mempty
  , ppOutput          = hPutStrLn xmproc
  , ppLayout          = id -- drop 18
  , ppTitle           = xmobarColor foreground mempty . shorten 85
  , ppHidden          = \ws -> if ws == "NSP" then mempty else ws
  , ppHiddenNoWindows = const mempty
  }

myConfig xmproc = def
    { layoutHook         = myLayoutHook
    , manageHook         = myManageHook <> manageHook def
    , handleEventHook    = restartEventHook
    , logHook            = myLogHook xmproc
    , modMask            = mod4Mask
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , startupHook        = myStartupHook
    , borderWidth        = myBorder
    }

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar-solomon"
  xmonad . ewmhFullscreen . ewmh . pagerHints . docks . withNavigation2DConfig myNav2DConf $ myConfig xmproc
