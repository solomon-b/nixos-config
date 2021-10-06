{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Prompt where

import           XMonad
import           XMonad.Prelude
import           XMonad.Util.Font
import           XMonad.Util.Types
import           XMonad.Util.XSelection (getSelection)
import qualified XMonad.StackSet as W

import           Control.Exception (bracket, finally)
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Reader
import           Data.IORef
import           System.IO.Unsafe

-- | The completion windows in its entirety.
data ComplWindowDim = ComplWindowDim
  { cwX         :: !Position    -- ^ Starting x position
  , cwY         :: !Position    -- ^ Starting y position
  , cwWidth     :: !Dimension   -- ^ Width of the entire prompt
  , cwRowHeight :: !Dimension   -- ^ Height of a single row
  , cwCols      :: ![Position]  -- ^ Starting position of all columns
  , cwRows      :: ![Position]  -- ^ Starting positions of all rows
  }
  deriving (Eq)

data XPState = XPS
  { dpy :: Display
  , rootw :: !Window
  , win :: !Window
  , screen :: !Rectangle
  , winWidth :: !Dimension
  , complWinDim :: Maybe ComplWindowDim
  , complWin :: IORef (Maybe Window)
  , showComplWin :: Bool
  , gcon :: GC
  , fontS :: !XMonadFont
  , config :: XPConfig
  , done :: Bool
  , numlockMask :: KeyMask
  }

data XPConfig = XPC
  { position              :: XPPosition   -- ^ Position: 'Top', 'Bottom', or 'CenteredAt'
  , height                :: !Dimension   -- ^ Window height
  , showCompletionOnTab   :: Bool         -- ^ Only show list of completions when Tab was pressed
  }

initState :: Display -> Window -> Window -> Rectangle
          -> GC -> XMonadFont -> XPConfig -> KeyMask -> Dimension -> XPState
initState d rw w s gc fonts c nm width =
  XPS { dpy = d
      , rootw = rw
      , win = w
      , screen = s
      , winWidth = width
      , complWinDim = Nothing
      , complWin = unsafePerformIO (newIORef Nothing)
      , showComplWin = not (showCompletionOnTab c)
      , gcon = gc
      , fontS = fonts
      , Prompt.config = c
      , done = False
      , numlockMask = nm
      }

data XPPosition = Top
                | Bottom
                -- | Prompt will be placed in the center horizontally and
                --   in the certain place of screen vertically. If it's in the upper
                --   part of the screen, completion window will be placed below(like
                --   in 'Top') and otherwise above(like in 'Bottom')
                | CenteredAt { xpCenterY :: Rational
                             -- ^ Rational between 0 and 1, giving
                             -- y coordinate of center of the prompt relative to the screen height.
                             , xpWidth  :: Rational
                             -- ^ Rational between 0 and 1, giving
                             -- width of the prompt relatave to the screen width.
                             }
                  deriving (Show,Read)

-- | Create the prompt window.
createPromptWin :: Display -> Window -> XPConfig -> Rectangle -> Dimension -> IO Window
createPromptWin dpy' rootw' XPC{ height } scn width = do
  w <- mkUnmanagedWindow dpy' (defaultScreenOfDisplay dpy') rootw'
                      (rect_x scn + 0) (rect_y scn + 0) width height
  setClassHint dpy' w (ClassHint "xmonad-prompt" "xmonad")
  mapWindow dpy' w
  return w

-- | Creates a window with the attribute override_redirect set to True.
-- Windows Managers should not touch this kind of windows.
mkUnmanagedWindow :: Display -> Screen -> Window -> Position
                  -> Position -> Dimension -> Dimension -> IO Window
mkUnmanagedWindow d s rw x y w h = do
  let visual = defaultVisualOfScreen s
      attrmask = cWOverrideRedirect
  allocaSetWindowAttributes $
         \attributes -> do
           set_override_redirect attributes True
           createWindow d rw x y w h 0 (defaultDepthOfScreen s)
                        inputOutput visual attrmask attributes

mkXPromptImplementation :: XPConfig -> X XPState
mkXPromptImplementation conf = do
  XConf { display = d, theRoot = rw } <- ask
  s <- gets $ screenRect . W.screenDetail . W.current . windowset
  numlock <- gets numberlockMask
  fs <- initXMF "xft:Meslo LG M:style=Regular:size=12"
  let width = getWinWidth s (position conf)
  st' <- io $
    bracket
      (createPromptWin d rw conf s width)
      (destroyWindow d)
      (\w ->
         bracket
           (createGC d w)
           (freeGC d)
           (\gc -> do
               selectInput d w $ exposureMask .|. keyPressMask
               setGraphicsExposures d gc False
               let st = initState d rw w s gc fs conf numlock width
               runXP st
           ))
  releaseXMF fs
  pure st'
  where
    getWinWidth :: Rectangle -> XPPosition -> Dimension
    getWinWidth scr = \case
      CenteredAt{xpWidth} -> floor $ fi (rect_width scr) * xpWidth
      _ -> rect_width scr

type XP = StateT XPState IO

runXP :: XPState -> IO XPState
runXP st@XPS{dpy = d, win = w} = do
  bracket
    (grabKeyboard d w True grabModeAsync grabModeAsync currentTime)
    (const $ ungrabKeyboard d currentTime)
    (\status ->
       execStateT
         (when (status == grabSuccess) $ do
             updateWindows
             eventLoop handleMain evDefaultStop)
         st
       `finally` (traverse_ (destroyWindow d) =<< readIORef (complWin st))
       `finally` sync d False)

--- | Update all prompt windows.
updateWindows :: XP ()
updateWindows = undefined

type KeyStroke = (KeySym, String)

-- | Main event "loop". Gives priority to events from the state's event buffer.
eventLoop :: (KeyStroke -> Event -> XP ())
          -> XP Bool
          -> XP ()
eventLoop handle stopAction = undefined

-- | Prompt event handler for the main loop. Dispatches to input, completion
-- and mode switching handlers.
handleMain :: KeyStroke -> Event -> XP ()
handleMain = undefined

-- | Default event loop stop condition.
evDefaultStop :: XP Bool
evDefaultStop = gets done
