{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

-- TODO:
-- - Setup dashboard view with widgets
-- - Cleanup imports

--------------------------------------------------------------------------------

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List (intersperse, isInfixOf, nub)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Monoid (All (..))
import GHC.IO (unsafeInterleaveIO)
import System.Exit (exitSuccess)
import System.IO (Handle, hGetLine, hIsEOF, hPutStrLn)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc)
import XMonad qualified
import XMonad.Actions.Commands (defaultCommands)
import XMonad.Actions.CopyWindow (copyToAll, kill1, killAllOtherCopies)
import XMonad.Actions.Navigation2D (Navigation2DConfig (..), centerNavigation, lineNavigation, singleWindowRect, windowGo, windowSwap, withNavigation2DConfig)
import XMonad.Actions.Promote (promote)
import XMonad.Core (recompile)
import XMonad.Hooks.DynamicLog (PP (..), dynamicLogWithPP, shorten, xmobarColor, xmobarPP)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers ((~?))
import XMonad.Hooks.StatusBar (StatusBarConfig, statusBarProp, withSB)
import XMonad.Hooks.TaffybarPagerHints (pagerHints)
import XMonad.Layout.Decoration (ModifiedLayout)
import XMonad.Layout.Gaps (Gaps, gaps)
import XMonad.Layout.MultiToggle (Toggle (..), mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (..))
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders (SetsAmbiguous (..), noBorders)
import XMonad.Layout.PerScreen (ifWider)
import XMonad.Layout.Renamed (Rename (..), renamed)
import XMonad.Layout.ResizableTile (ResizableTall (..))
import XMonad.Layout.SimpleFloat (shrinkText)
import XMonad.Layout.Simplest (Simplest (..))
import XMonad.Layout.Spacing (Spacing, spacing)
import XMonad.Layout.SubLayouts (GroupMsg (..), pullGroup, subLayout)
import XMonad.Layout.Tabbed (Direction2D (..), Theme (..), addTabs)
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Layout.WindowNavigation (windowNavigation)
import XMonad.Operations (restart)
import XMonad.Prompt (XPConfig (..), XPPosition (..), emacsLikeXPKeymap)
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.XMonad (xmonadPromptCT)
import XMonad.StackSet qualified as W
import XMonad.Util.ExtensibleState qualified as XS
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.NamedScratchpad (NamedScratchpad (..), customFloating, namedScratchpadAction)
import XMonad.Util.Run (spawnPipe)

--------------------------------------------------------------------------------
-- Theme

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

active :: String
active = red

activeWarn :: String
activeWarn = blue

inactive :: String
inactive = orange

focusColor :: String
focusColor = red

unfocusColor :: String
unfocusColor = orange

myFont :: String
myFont = "xft:Meslo LG M:style=Regular:size=12"

myNormalBorderColor :: String
myNormalBorderColor = blue

myFocusedBorderColor :: String
myFocusedBorderColor = red

myTabTheme :: Theme
myTabTheme =
  XMonad.def
    { fontName = myFont,
      activeColor = altBackground,
      inactiveColor = background,
      activeBorderColor = altBackground,
      inactiveBorderColor = background,
      activeTextColor = foreground,
      inactiveTextColor = comment
    }

--------------------------------------------------------------------------------
-- Layouts

gap :: Int
gap = 4

topbar :: Integer
topbar = 10

myBorder :: XMonad.Dimension
myBorder = 1

prompt :: Integer
prompt = 20

status :: Integer
status = 20

mySpacing :: l a -> ModifiedLayout Spacing l a
mySpacing = spacing gap

myGaps :: l a -> ModifiedLayout Gaps l a
myGaps = gaps [(U, gap), (D, gap), (L, gap), (R, gap)]

trimNamed :: Int -> String -> l a -> ModifiedLayout Rename l a
trimNamed w n = renamed [CutWordsLeft w, PrependWords n]

suffixed :: String -> l a -> ModifiedLayout Rename l a
suffixed n = renamed [AppendWords n]

trimSuffixed :: Int -> String -> l a -> ModifiedLayout Rename l a
trimSuffixed w n = renamed [CutWordsRight w, AppendWords n]

data TABBED = TABBED
  deriving (Show, Read, Eq, XMonad.Typeable)

--instance Transformer MIRROR Window where
--    transform _ x k = k (Mirror x) (\(Mirror x') -> x')

myLayoutHook = avoidStruts $ mirrorToggle $ fullScreenToggle $ flex XMonad.||| tabs

data FocusedOnly = FocusedOnly
  deriving (Show, Read)

instance SetsAmbiguous FocusedOnly where
  hiddens :: FocusedOnly -> XMonad.WindowSet -> XMonad.Rectangle -> Maybe (W.Stack XMonad.Window) -> [(XMonad.Window, XMonad.Rectangle)] -> [XMonad.Window]
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
    wideThreeCol = suffixed "Wide 3Col" (ThreeColMid 1 (1 / 20) (1 / 2))
    wideLayouts = mySpacing . myGaps $ wideThreeCol XMonad.||| standardLayout
    standardLayout =
      mySpacing . myGaps . named "Std 2/3" $
        ResizableTall 1 (1 / 20) (2 / 3) [] --  ||| Mirror (ResizableTall 1 (1/20) (2/3) [])

myBrowser = "/usr/bin/firefox"

myTerminal = "termonad"

myLauncher = XMonad.Prompt.Shell.shellPrompt promptConfig

myWorkspaces = ["1:term", "2:web", "3:slack"] ++ map show [4 .. 9]

myManageHook =
  XMonad.composeAll
    [ XMonad.className XMonad.=? "Firefox" XMonad.--> XMonad.doShift "2:web",
      XMonad.className XMonad.=? "Slack" XMonad.--> XMonad.doShift "3:slack",
      XMonad.className XMonad.=? "trayer" XMonad.--> XMonad.doIgnore,
      manageDocks
    ]

--------------------------------------------------------------------------------
-- Prompts

promptConfig :: XPConfig
promptConfig =
  XMonad.def
    { position = Top,
      height = 20,
      font = myFont,
      bgColor = background,
      fgColor = orange,
      fgHLight = "#d33682",
      bgHLight = "#073642",
      promptBorderWidth = 0,
      maxComplRows = Just 12,
      alwaysHighlight = True,
      promptKeymap = emacsLikeXPKeymap,
      searchPredicate = isInfixOf `on` map toLower
    }

-- | X Session logout and system shutdown/reboot prompt
exitPrompt :: XMonad.X ()
exitPrompt = xmonadPromptCT "Exit" commands promptConfig
  where
    commands =
      [ ("1: Logout", XMonad.io exitSuccess),
        ("2: Shutdown", XMonad.spawn "systemctl poweroff"),
        ("3: Reboot", XMonad.spawn "systemctl reboot"),
        ("4: Reload XMonad", restart)
      ]
    restart = do
      XMonad.spawn "pkill trayer"
      XMonad.spawn "xmonad --restart"

-- | Kill the focused window
closeWindowPrompt :: XMonad.X ()
closeWindowPrompt = confirmPrompt promptConfig "Close Window" kill1

-- | Screenshot prompt
scrotPrompt :: XMonad.X ()
scrotPrompt = xmonadPromptCT "Screenshot Options" commands promptConfig
  where
    commands =
      [ ("1: Capture Screen", XMonad.spawn "scrot"),
        ("2: Capture Selection", XMonad.spawn "scrot -s"),
        ("3: Capture All Screens", XMonad.spawn "scrot -m"),
        ("4: Capture with 3 second countdown", XMonad.spawn "scrot -d 3 -c")
      ]

layoutPrompt :: XMonad.X ()
layoutPrompt = xmonadPromptCT "Window Commands" commands promptConfig
  where
    commands =
      [ ("1: Tabbed Layout", XMonad.sendMessage (XMonad.JumpToLayout "Tabs")),
        ("2: Two Column Layout", XMonad.sendMessage (XMonad.JumpToLayout "Flex")),
        ("3: Float/Unfloat window", floatFocused)
      ]

-- | Simple prompt for launching scratchpads
scratchpadPrompt :: XMonad.X ()
scratchpadPrompt = xmonadPromptCT "Scratchpads" commands promptConfig
  where
    commands =
      [ ("1: Personal Calendar", namedScratchpadAction scratchpads "personalCal")
      -- , ("2: Hasura Calendar", namedScratchpadAction scratchpads "hasuraCal")
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
  { _unit :: String,
    _load :: String,
    _active :: String,
    _sub :: String,
    _description :: String
  }
  deriving (Show)

mkSystemUnit :: [String] -> Maybe SystemUnit
mkSystemUnit [a, b, c, d, e] = Just $ SystemUnit a b c d e
mkSystemUnit _ = Nothing

-- | Fetch systemd user units.
--
-- TODO:
fetchUnits :: IO [SystemUnit]
fetchUnits = do
  let p = proc "systemctl" ["--user"]
  (_, hout, _, _) <- createProcess p {std_out = CreatePipe}
  case hout of
    Just hout' -> do
      res <- words <$> hGetLine hout'
      res1 <- mapMaybe (mkSystemUnit . words) <$> hGetLines hout'
      print res
      pure res1
    _ -> pure []

readEmojis :: IO [String]
readEmojis = do
  let p = proc "cat" ["/home/solomon/.local/share/emoji"] 
  (_, hout, _, _) <- createProcess p {std_out = CreatePipe}
  case hout of
    Just hout' -> hGetLines hout'
    _ -> pure []

emojiPrompt :: XMonad.X ()
emojiPrompt = do
  let action e = XMonad.spawn $ "echo " <> e <> " | xclip -sel clip"
  emojis <- fmap (\e -> (e, action e)) <$> XMonad.liftIO readEmojis
  xmonadPromptCT "Emojis" emojis promptConfig
       
--------------------------------------------------------------------------------
-- Scratchpads

-- | TODO: look into other useful scratchpads.
-- https://github.com/thcipriani/dotfiles/blob/master/xmonad/xmonad.hs#L99-L101
scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS
      "personalCal"
      "surf 'calendar.google.com/?authuser=ssbothwell@gmail.com'"
      -- TODO: Figure out why `title` isn't working as expected here
      (XMonad.className XMonad.=? "Surf") -- <&&> title ~? "Google Calendar")
      (customFloating $ W.RationalRect 0.9 0.9 0.9 0.9),
    NS
      "hasuraCal"
      "surf 'calendar.google.com/?authuser=solomon@hasura.io'"
      ((XMonad.className ~? "Surf") XMonad.<&&> XMonad.title ~? "Hasura")
      (customFloating $ W.RationalRect 0.9 0.9 0.9 0.9)
  ]

--------------------------------------------------------------------------------
-- Dashboard State

data DashboardState = On | Off
  deriving Eq

instance XMonad.ExtensionClass DashboardState where
  initialValue = Off


toggleDashboard :: XMonad.X ()
toggleDashboard = do
  XS.get >>= \case
    On -> do
      XMonad.spawn "/home/solomon/.config/eww/close_dashboard" 
      XS.put Off
    Off -> do
      XMonad.spawn "/home/solomon/.config/eww/open_dashboard" 
      XS.put On

--------------------------------------------------------------------------------
-- Keybindings

workSpaceNav :: XMonad.XConfig a -> [(String, XMonad.X ())]
workSpaceNav c = do
  (i, j) <- zip (map show [1 .. 9]) $ XMonad.workspaces c
  (m, f) <- [("M-", W.greedyView), ("M-S-", W.shift)]
  return (m ++ i, XMonad.windows $ f j)

myKeys :: XMonad.XConfig a -> M.Map (XMonad.KeyMask, XMonad.KeySym) (XMonad.X ())
myKeys c =
  mkKeymap c $
    -- System
    [ ("M-<Space> q", exitPrompt),
      ("M-<Space> w", layoutPrompt),
      ("M-<Space> <Space>", myLauncher),
      ("M-<Space> e", emojiPrompt),
      ("M-<Space> p", scratchpadPrompt),
      ("M-<Backspace>", closeWindowPrompt),
      ("M-S-<Backspace>", XMonad.withUnfocused XMonad.killWindow),
      ("<XF86AudioMedia>", toggleDashboard),
      ("<XF86AudioMute>", toggleMute),
      ("<XF86AudioRaiseVolume>", volumeUp),
      ("<XF86AudioLowerVolume>", volumeDown),
      ("<XF86MonBrightnessUp>", XMonad.spawn "brightnessctl set 5%+"),
      ("<XF86MonBrightnessDown>", XMonad.spawn "brightnessctl set 5%-"),
      ("M-<XF86AudioMute>", toggleDunst >> toggleMute),
      ("<Print>", scrotPrompt),
      ("C-<Space>", dunstClose),
      ("C-S-<Space>", dunstCloseAll)
    ]
      <>
      -- Navigate between windows
      [ ("M-j", windowGo D False),
        ("M-k", windowGo U False),
        ("M-h", windowGo L False),
        ("M-l", windowGo R False),
        -- Navigate between tabs
        ("M-;", XMonad.windows W.focusUp),
        ("M-'", XMonad.windows W.focusDown),
        -- Shift tabs
        ("M-S-;", XMonad.windows W.swapUp),
        ("M-S-'", XMonad.windows W.swapDown),
        -- Swap adjacent windows
        ("M-S-j", windowSwap D False),
        ("M-S-k", windowSwap U False),
        ("M-S-h", windowSwap L False),
        ("M-S-l", windowSwap R False),
        -- Shrink/Expand windows
        ("M-[", XMonad.sendMessage XMonad.Shrink),
        ("M-]", XMonad.sendMessage XMonad.Expand),
        ("M-r", XMonad.sendMessage $ Toggle MIRROR),
        -- Float/Sink floated window
        ("M-t", floatFocused),
        ("M-C-t", XMonad.withFocused toggleSticky),
        -- Full Screen a window
        ("M-<F11>", XMonad.sendMessage $ Toggle FULL),
        -- Promote window to master
        ("M-b", promote),
        -- "merge with sublayout"
        ("M-C-h", XMonad.sendMessage . pullGroup $ L),
        ("M-C-l", XMonad.sendMessage . pullGroup $ R),
        ("M-C-j", XMonad.sendMessage . pullGroup $ D),
        ("M-C-k", XMonad.sendMessage . pullGroup $ U),
        -- Unmerge a window
        ("M-g", XMonad.withFocused (XMonad.sendMessage . UnMerge))
      ]
      <> workSpaceNav c
      <> [ ("M-<Return>", XMonad.spawn myTerminal), -- Launch Terminal
           ("M-\\", XMonad.spawn myBrowser), -- Launch Browser
           ("M-p", myLauncher) -- Launch DMenu
         ]
  where
    toggleDunst = XMonad.spawn "dunstctl set-paused toggle"
    toggleMute = XMonad.spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"
    dunstClose = XMonad.spawn "dunstctl close"
    dunstCloseAll = XMonad.spawn "dunstctl close-all"
    volumeUp = XMonad.spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%"
    volumeDown = XMonad.spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%"

toggleSticky :: XMonad.Window -> XMonad.X ()
toggleSticky w = XMonad.windows $ \s ->
  if M.member w (W.floating s)
    then copyToAll s
    else s

toggleFloat :: XMonad.Window -> XMonad.X ()
toggleFloat w = XMonad.windows $ \s ->
  if M.member w (W.floating s)
    then W.sink w s
    else W.float w (W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)) s

floatFocused :: XMonad.X ()
floatFocused = do
  XMonad.withFocused toggleFloat
  killAllOtherCopies

myNav2DConf :: Navigation2DConfig
myNav2DConf =
  XMonad.def
    { defaultTiledNavigation = centerNavigation,
      floatNavigation = centerNavigation,
      screenNavigation = lineNavigation,
      layoutNavigation = pure ("Full", centerNavigation),
      unmappedWindowRect = pure ("Full", singleWindowRect)
    }

myMouseBindings :: XMonad.XConfig l -> M.Map (XMonad.KeyMask, XMonad.Button) (XMonad.Window -> XMonad.X ())
myMouseBindings XMonad.XConfig {XMonad.modMask = modm} =
  M.fromList
    [ ((modm, XMonad.button1), \w -> XMonad.focus w >> XMonad.mouseMoveWindow w >> XMonad.windows W.shiftMaster), -- Set window to float and move by dragging
      ((modm XMonad..|. XMonad.controlMask, XMonad.button1), \w -> XMonad.focus w >> XMonad.mouseResizeWindow w >> XMonad.windows W.shiftMaster) -- Set window to float and resize by dragging
    ]

--------------------------------------------------------------------------------
-- Main

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe path =
  fmap (either (const Nothing) Just) $ try @SomeException $ readFile path

myStartupHook :: XMonad.X ()
myStartupHook = do
  commands <- XMonad.liftIO $ readFileMaybe "/home/solomon/.startup"
  traverse_ (traverse_ XMonad.spawn) $ fmap lines commands

restartEventHook :: XMonad.Event -> XMonad.X All
restartEventHook = \case
  XMonad.ClientMessageEvent {XMonad.ev_message_type} -> do
    atom <- XMonad.getAtom "XMONAD_RESTART"
    when (ev_message_type == atom) $ XMonad.restart "xmonad-solomon" True
    pure $ All True
  _ -> pure $ All True

statusBarConfig :: StatusBarConfig
statusBarConfig =
  statusBarProp "xmobar-solomon" $
    pure $
      xmobarPP
        { ppCurrent = xmobarColor yellow mempty,
          ppLayout = id, -- drop 18
          ppTitle = xmobarColor foreground mempty . shorten 85,
          ppHidden = \ws -> if ws == "NSP" then mempty else ws,
          ppHiddenNoWindows = const mempty
        }

myConfig =
  withSB statusBarConfig $
    XMonad.def
      { XMonad.layoutHook = myLayoutHook,
        XMonad.manageHook = myManageHook <> XMonad.manageHook XMonad.def,
        XMonad.handleEventHook = restartEventHook,
        XMonad.modMask = XMonad.mod4Mask,
        XMonad.keys = myKeys,
        XMonad.mouseBindings = myMouseBindings,
        XMonad.workspaces = myWorkspaces,
        XMonad.normalBorderColor = myNormalBorderColor,
        XMonad.focusedBorderColor = myFocusedBorderColor,
        XMonad.startupHook = myStartupHook,
        XMonad.borderWidth = myBorder
      }

main :: IO ()
main =
  XMonad.xmonad . ewmhFullscreen . ewmh . pagerHints . docks . withNavigation2DConfig myNav2DConf $ myConfig
