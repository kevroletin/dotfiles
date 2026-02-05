{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

import Control.Exception as E
import qualified Data.List as L
import qualified Data.Map as M
import Data.Semigroup (All)
import qualified Data.Text as T
import Data.Traversable
import Graphics.X11.ExtraTypes.XF86
import MarkEwmhFullscreen
import MaximizeFocused
import MyNoBorders
import System.Clipboard (getClipboardString)
import System.Directory (doesDirectoryExist, getHomeDirectory)
import System.Exit
import System.FilePath ((</>))
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CycleWS
import XMonad.Actions.Minimize
import XMonad.Actions.RotSlaves
import XMonad.Actions.UpKeys
import XMonad.Actions.UpdatePointer
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Minimize
import XMonad.Layout.BoringWindows
import qualified XMonad.Layout.BoringWindows as BW

-- import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ServerMode
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.MagicFocus
import XMonad.Layout.Minimize
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.ZoomRow
import XMonad.Prelude (fromMaybe, listToMaybe, (>=>))
import qualified XMonad.StackSet as W
import qualified XMonad.Util.Hacks as Hacks
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Run (
  runProcessWithInput,
  safeSpawn,
 )
import XMonad.Util.SpawnOnce

stripText :: String -> String
stripText = T.unpack . T.strip . T.pack

dirFromClipboard :: IO String
dirFromClipboard =
  getClipboardString >>= \case
    Nothing -> getHomeDirectory
    Just x' -> do
      x <- expandTilda (stripText x')
      doesDirectoryExist x >>= \case
        True -> pure x
        False -> getHomeDirectory
 where
  expandTilda path
    | "~/" `L.isPrefixOf` path = do
        home <- getHomeDirectory
        pure (home </> drop 2 path)
    | "~" == path = getHomeDirectory
    | otherwise = pure path

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS
      "quake"
      "alacritty --class scratchpad-quake"
      (appName =? "scratchpad-quake")
      (customFloating $ W.RationalRect 0 (6 / 10) 1 (4 / 10))
  , NS
      "numen"
      ("alacritty --class scratchpad-numen --working-directory $HOME -e " ++ numenTailCmd)
      (appName =? "scratchpad-numen")
      (customFloating $ W.RationalRect (33 / 40) (9 / 20) (3 / 20) (5 / 10))
  ]
 where
  numenTailCmd = "sh -c 'echo > /tmp/phrases.log; tail -F /tmp/phrases.log & numen --phraselog /tmp/phrases.log'"

quakeAct :: X ()
quakeAct =
  customRunNamedScratchpadAction
    ( \_ -> do
        x <- liftIO dirFromClipboard
        safeSpawn
          "alacritty"
          [ "--working-directory"
          , x
          , "--class"
          , "scratchpad-quake"
          , "-o"
          , "window.opacity=1"
          ]
    )
    scratchpads
    "quake"

type KeyCombination = (KeyMask, KeySym)

type KeyBinding = (KeyCombination, X ())

ifProcessRuns :: String -> X () -> X () -> X ()
ifProcessRuns pname a b =
  do
    out <- runProcessWithInput "pgrep" [pname] ""
    if null out then b else a

killOrSpawn :: String -> [String] -> X ()
killOrSpawn pname args =
  ifProcessRuns pname (safeSpawn "pkill" [pname]) (safeSpawn pname args)

openInEmacs :: [String] -> X ()
openInEmacs args = ifProcessRuns "emacs" viaClient viaEmacs
 where
  viaClient = safeSpawn "emacsclient" ("--no-wait" : args)
  viaEmacs = safeSpawn "emacs" args

_stopWhisper :: X ()
_stopWhisper = do
  safeSpawn "/bin/bash" ["-c", "echo load ~/.config/numen/phrases/*.phrases| numenc"]
  safeSpawn "flatpak" ["run", "net.mkiol.SpeechNote", "--action", "stop-listening"]

_openEmacsAgenda :: X ()
_openEmacsAgenda = openInEmacs ["~/org/personal/gtd.org"]

spawnAlacritty :: X ()
spawnAlacritty = do
  dir <- liftIO dirFromClipboard
  safeSpawn "alacritty" ["--working-directory", dir]

openObsidian :: X ()
openObsidian =
  ifProcessRuns
    "obsidian"
    (return ())
    (safeSpawn "obsidian" ["obsidian://open?vault=share&file=Dashboard"])

-- pause numen and start voxtype, so that numen doesn't jump around while transcribing speech
voxtypeStart :: X ()
voxtypeStart = do
  spawn "~/Scratch/rust/voxtype/target/release/voxtype record start"
  spawn "notify-send 'voxtype record start'"
  -- pause numen
  safeSpawn "/bin/bash" ["-c", "echo load ~/.config/numen/phrases/empty.phrases | numenc"]

voxtypeStop :: X ()
voxtypeStop = do
  spawn "~/Scratch/rust/voxtype/target/release/voxtype record stop"
  spawn "notify-send 'voxtype record stop'"
  -- resume numen
  safeSpawn "/bin/bash" ["-c", "echo load ~/.config/numen/phrases/*.phrases | numenc"]

centreRect :: W.RationalRect
centreRect = W.RationalRect 0.2 0.05 0.6 0.9

-- If the window is floating then (f), if tiled then (n)
floatOrNot :: X () -> X () -> X ()
floatOrNot f n = withFocused $ \windowId -> do
  floats <- gets (W.floating . windowset)
  if windowId `M.member` floats -- if the current window is floating...
    then f
    else n

-- Centre and float a window (retain size)
_centreFloat :: Window -> X ()
_centreFloat win = do
  (_, W.RationalRect _x _y w h) <- floatLocation win
  windows $ W.float win (W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h)
  return ()

-- Float a window in the centre
centreFloat' :: Window -> X ()
centreFloat' w = windows $ W.float w centreRect

-- Make a window my 'standard size' (half of the screen) keeping the centre of the window fixed
_standardSize :: Window -> X ()
_standardSize win = do
  (_, W.RationalRect x y _w _h) <- floatLocation win
  windows $ W.float win (W.RationalRect x y 0.5 0.5)
  return ()

-- Float and centre a tiled window, sink a floating window
toggleFloat :: X ()
toggleFloat = floatOrNot (withFocused $ windows . W.sink) (withFocused centreFloat')

-- Configuring X setting turned out to be complicated due to startup order. Some daemon overrides settings during user startup
-- and playing with systemctl startup sequence didn't help. As a workaround, this script has 5sec sleep and we run it here
-- asynchronously
-- + disable repeating (bouncing) ScrollLock key
-- + enable russian layout
configureXset :: X ()
configureXset = do spawnOnce "~/.xmonad/configure-xset"

toggleLayout :: X ()
toggleLayout = do
  wset <- gets windowset
  let desc = description . W.layout . W.workspace . W.current $ wset
  let nextLayout = if desc == "SFull" then "STall" else "SFull"
  sendMessage (JumpToLayout nextLayout)

-- mod1Mask - alt
-- mod4Mask - win
-- https://xmonad.github.io/xmonad-docs/xmonad/src/XMonad.Config.html
keysToAdd :: XConfig l -> [KeyBinding]
keysToAdd x =
  [ -- quit, or restart
    ((modMask x, xK_u), withFocused minimizeWindow) -- Hide focused window
  , ((modMask x .|. shiftMask, xK_q), io exitSuccess)
  , ((modMask x, xK_q), spawn "xmonad --recompile && xmonad --restart")
  , ((modMask x .|. shiftMask, xK_c), kill1) -- close only focused copied window
  , ((modMask x .|. shiftMask .|. controlMask, xK_c), kill)
  , -- hjkl
    ((modm .|. shiftMask .|. controlMask, xK_j), windows W.swapDown)
  , ((modm .|. shiftMask .|. controlMask, xK_k), windows W.swapUp)
  , ((modm .|. shiftMask, xK_j), rotAllDown)
  , ((modm .|. shiftMask, xK_k), rotAllUp)
  , ((modm, xK_j), BW.focusDown)
  , ((modm, xK_k), BW.focusUp)
  , ((modMask x, xK_m), BW.focusMaster) -- %! Move focus to the master window
  , ((modMask x, xK_Left), prevWS)
  , ((modMask x, xK_Right), nextWS)
  , ((modMask x .|. shiftMask, xK_Left), shiftToPrev)
  , ((modMask x .|. shiftMask, xK_Right), shiftToNext)
  , ((modMask x, xK_h), prevWS)
  , ((modMask x, xK_l), nextWS)
  , ((modMask x, xK_p), unGrab >> spawn "rofi -show run")
  , ((modMask x .|. shiftMask, xK_p), unGrab >> spawn "~/.xmonad/which-key window-list --bring")
  , ((modMask x, xK_v), unGrab >> spawn "~/.xmonad/which-key window-list")
  , ((modMask x .|. shiftMask, xK_v), unGrab >> spawn "rofi -show window")
  , ((modMask x .|. controlMask, xK_Return), safeSpawn "emacs" [])
  , -- Mod + Tab enters "cycle through history" mode.
    ((modMask x, xK_Tab), cycleRecentWS [xK_Tab] xK_Left xK_Right)
  , -- Print screen
    ((controlMask, xK_Print), unGrab >> spawn "cd ~/Share/screenshots; sleep 0.2; scrot -s")
  , ((0, xK_Print), unGrab >> spawn "cd ~/Share/screenshots; scrot")
  , ((modMask x, xK_F1), spawn "xprop | xmessage -file -")
  , ((modMask x, xK_F3), openObsidian)
  , ((modMask x, xK_F4), killOrSpawn "redshift" [])
  , -- , ((modMask x, xK_b), sendMessage ToggleStruts)
    -- , ((modMask x .|. shiftMask, xK_b), do spawn "~/.xmonad/toggle-xmobar") -- kill xmobar
    -- lock screen
    ((modMask x, xK_z), do safeSpawn "xscreensaver-command" ["-lock"])
  , ((modMask x .|. shiftMask, xK_z), do spawn "sleep 1s; xset dpms force off")
  , -- leader key
    -- ((modMask x, xK_space), do spawn "~/.xmonad/which-key")

    ( (modMask x, xK_space)
    , do
        -- scientifically unproved attempt to eliminate potential slowdonw of starting which-key script
        -- which might become slower over time
        --
        unGrab >> spawn "setxkbmap -layout 'us'; rofi -input ~/.xmonad/which-key.list.txt -drun-use-desktop-cache -dmenu -window-title 'Which key' -cycle -matching regex -selected-row -filter '^' -auto-select | nu --stdin ~/.xmonad/which-key continue"
    )
  , ((modMask x .|. shiftMask .|. controlMask, xK_space), do spawn "~/.xmonad/which-key repeat")
  , -- cycle layouts
    -- ((modMask x .|. shiftMask, xK_space), toggleLayout)
    ((modMask x .|. shiftMask, xK_space), sendMessage NextLayout)
  , -- Float and enlarge selected window
    ((modMask x, xK_f), withFocused (sendMessage . maximizeFocusedRestore))
  , -- resizing the master/slave ratio
    ((modm, xK_comma), sendMessage Shrink)
  , ((modm, xK_period), sendMessage Expand)
  , ((modm, xK_bracketleft), sendMessage (IncMasterN (-1)))
  , ((modm, xK_bracketright), sendMessage (IncMasterN 1))
  , -- scratchpads
    ((modm, xK_Escape), quakeAct)
  , ((modm, xK_grave), quakeAct)
  , ((modm .|. shiftMask, xK_n), namedScratchpadAction scratchpads "numen")
  , -- See Graphics.X11.ExtraTypes.XF86
    ((0, xF86XK_AudioLowerVolume), spawn "~/.xmonad/knob-action -")
  , ((0, xF86XK_AudioRaiseVolume), spawn "~/.xmonad/knob-action +")
  , ((0, xF86XK_AudioMute), spawn "~/.xmonad/knob-action '!'")
  , ((0, xK_Scroll_Lock), voxtypeStart) -- see myUpKeys for the stop action
  -- Pin(unpin) window to all workspaces
  , ((modm .|. shiftMask, xK_a), windows copyToAll)
  , ((modm, xK_a), killAllOtherCopies)
  , ((modm .|. shiftMask, xK_Return), spawnAlacritty)
  , -- Default keybindings
    ((modMask x, xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
  , ((modMask x, xK_t), toggleFloat)
  ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [ ((m .|. modMask x, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces x) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
    ]
    -- ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    -- [ ((m .|. modMask x, key), screenWorkspace sc >>= flip whenJust (windows . f))
    -- \| (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
    -- , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    -- ]
    -- copy to workspace
    ++ [ ((m .|. modm, k), windows $ f i)
       | (i, k) <- zip (workspaces x) [xK_1 ..]
       , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]
       ]
 where
  modm = modMask x

-- Run this command to disable key repeat
-- xset -r 78
myUpKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
myUpKeys _conf =
  M.fromList
    [ ((0, xK_Scroll_Lock), voxtypeStop)
    ]

-- Modify default key bindings scheme
myKeys :: XConfig Layout -> M.Map KeyCombination (X ())
myKeys x = M.fromList (keysToAdd x)

fstOr x [] = x
fstOr _ (x : _) = x

-- modified https://xmonad.github.io/xmonad-docs/xmonad-contrib/src/XMonad.Util.NamedWindows.html#getNameWMClass
_getNameWMClass :: Window -> X String
_getNameWMClass w =
  withDisplay $ \d ->
    -- TODO, this code is ugly and convoluted -- clean it up
    do
      let getIt = bracket getProp (xFree . tp_value) copy
          getProp = getTextProperty d w wM_CLASS
          copy prop =
            fromMaybe "" . listToMaybe <$> wcTextPropertyToTextList d prop
      io $
        getIt `E.catch` \(SomeException _) ->
          resName <$> getClassHint d w

tab theme fg bg =
  wrap (xmobarColor bg (th_bg theme) "\xE0B6") (xmobarColor bg (th_bg theme) "\xE0B4") . xmobarColor fg bg
activeTab theme = tab theme (th_activeTabFg theme) (th_activeTabBg theme)
inactiveTab theme = tab theme (th_inactiveTabFg theme) (th_inactiveTabBg theme)
workspaceTab theme = tab theme (th_workspaceTabFg theme) (th_workspaceTabBg theme)

logNumWin :: Theme -> Logger
logNumWin theme = withWindowSet $ \ws -> do
  case (W.stack . W.workspace . W.current) ws of
    Nothing -> pure (Just "")
    Just st -> do
      let items = length (W.up st) + length (W.down st) + 1
      let availableLen = 90
      let upperLimit = min 30 (max 5 (availableLen `div` items)) - 2

      xs@W.Stack{up, focus, down} <- readNames upperLimit st

      let up' = fmap (inactiveTab theme) up
      let down' = fmap (inactiveTab theme) down
      let before = if null up then "" else (L.intercalate "" (reverse up'))
      let after = if null down then "" else (L.intercalate "" down')

      let res = before ++ activeTab theme focus ++ after
      pure (Just res)
 where
  prettyNameM :: Int -> Window -> X String
  prettyNameM upperLimit w = do
    name <- show <$> getName w
    cls <- _getNameWMClass w
    pure (shorten upperLimit $ cls ++ " " ++ name)

  readNames :: Int -> W.Stack Window -> X (W.Stack String)
  readNames upperLimit = traverse (prettyNameM upperLimit)

mySB :: Theme -> StatusBarConfig
mySB theme =
  res
    { sbStartupHook = spawn "systemctl --user restart xmobar.service"
    , sbCleanupHook = pure () -- spawn "systemctl --user stop xmobar.service"
    }
 where
  res = statusBarProp "xmobar ~/.xmobarrc" (copiesPP (wrap "✦" "") myPP)
  myPP =
    xmobarPP
      { ppCurrent = workspaceTab theme
      , ppTitle = const ""
      , ppHidden = noScratchPad
      , ppSep = " / "
      , ppLayout = id
      , ppExtras = [logNumWin theme]
      }
  noScratchPad ws = if ws == "NSP" then "" else ws

------------------------------------------------------------------------
-- External commands
myCommands :: [(String, X ())]
myCommands =
  [ ("decrease-master-size", sendMessage Shrink)
  , ("increase-master-size", sendMessage Expand)
  , ("decrease-master-count", sendMessage $ IncMasterN (-1))
  , ("increase-master-count", sendMessage $ IncMasterN 1)
  , ("focus-prev", windows W.focusUp)
  , ("focus-next", windows W.focusDown)
  , ("focus-master", windows W.focusMaster)
  , ("swap-with-prev", windows W.swapUp)
  , ("swap-with-next", windows W.swapDown)
  , ("swap-with-master", windows W.swapMaster)
  , ("kill-window", kill)
  , --
    ("layout-next", toggleLayout)
  , ("layout-set-full", sendMessage (JumpToLayout "Full"))
  , ("layout-set-sfull", sendMessage (JumpToLayout "SFull"))
  , ("layout-set-stall", sendMessage (JumpToLayout "STall"))
  , ("layout-toggle-focused-maximize", withFocused (sendMessage . maximizeFocusedRestore))
  , ("layout-toggle-float", toggleFloat)
  , ("toggle-docks", sendMessage ToggleStruts)
  , --
    ("layout-spacing-inc", incScreenWindowSpacing defSpacing)
  , ("layout-spacing-dec", decScreenWindowSpacing defSpacing)
  , ("layout-spacing-set-1", setScreenWindowSpacing (defSpacing * 1))
  , ("layout-spacing-set-2", setScreenWindowSpacing (defSpacing * 2))
  , ("layout-spacing-set-3", setScreenWindowSpacing (defSpacing * 3))
  , ("layout-spacing-set-4", setScreenWindowSpacing (defSpacing * 4))
  , ("layout-spacing-set-5", setScreenWindowSpacing (defSpacing * 5))
  ,
    ( "layout-spacing-toggle"
    , do
        toggleScreenSpacingEnabled
        toggleWindowSpacingEnabled
    )
  , ("layout-hide-window", withFocused minimizeWindow)
  , ("layout-restore-hidden-windor", withLastMinimized maximizeWindowAndFocus)
  ]

myServerModeEventHook :: Event -> X All
myServerModeEventHook = serverModeEventHookCmd' $ return myCommands

_listMyServerCmds :: X ()
_listMyServerCmds = spawn ("echo '" ++ asmc ++ "' | xmessage -file -")
 where
  asmc = concat $ "Available commands:" : map (\(x, _) -> "    " ++ x) myCommands

defSpacing :: (Num a) => a
defSpacing = 6

data Theme = Theme
  { th_bg :: String
  , th_fg :: String
  , th_inactiveTabBg :: String
  , th_inactiveTabFg :: String
  , th_activeTabBg :: String
  , th_activeTabFg :: String
  , th_workspaceTabFg :: String
  , th_workspaceTabBg :: String
  , th_focusedBorder :: String
  , th_normalBorder :: String
  }
  deriving (Show)

darkTheme =
  Theme
    { th_fg = "#ccc6b7"
    , th_bg = "#000f13"
    , th_activeTabFg = "#000f13"
    , th_activeTabBg = "#ccc6b7"
    , th_inactiveTabFg = "#eee8d5"
    , th_inactiveTabBg = "#073642"
    , th_workspaceTabFg = "#000f13"
    , th_workspaceTabBg = "#ccc6b7"
    , th_focusedBorder = "#d33682"
    , th_normalBorder = "#586e75"
    }

lightTheme =
  Theme
    { th_fg = "#002b36"
    , th_bg = "#eee8d5"
    , th_inactiveTabFg = "#fdf6e3"
    , th_inactiveTabBg = "#93a1a1"
    , th_activeTabFg = "#fdf6e3"
    , th_activeTabBg = "#073642"
    , th_workspaceTabFg = "#fdf6e3"
    , th_workspaceTabBg = "#073642"
    , th_focusedBorder = "#dc322f"
    , th_normalBorder = "#586e75"
    }

main :: IO ()
main = do
  systemTheme <- runProcessWithInput "/bin/sh" ["-c", "timeout 2s gsettings get org.gnome.desktop.interface color-scheme || echo prefer-light"] []
  let theme = if "prefer-light" `L.isInfixOf` systemTheme then lightTheme else darkTheme
  xmonad
    . docks
    . withEasySB (mySB theme) defToggleStrutsKey
    . (\c -> useUpKeys (def{grabKeys = True, upKeys = myUpKeys c}) c)
    -- . ewmhFullscreen -- maximize windows which enabled full screen by themselves
    . ewmh
    $ desktopConfig
      { manageHook = manageDocks <+> manageHook desktopConfig <+> namedScratchpadManageHook scratchpads <+> myManageHook
      , layoutHook = myLayoutHook
      , logHook = do
          -- fadeInactiveLogHook 1
          -- mouse pointer follows focus
          -- https://hackage.haskell.org/package/xmonad-contrib-0.18.1/docs/XMonad-Actions-UpdatePointer.html
          updatePointer (0.5, 0.5) (0, 0)
      , modMask = mod4Mask
      , borderWidth = 1
      , focusedBorderColor = th_focusedBorder theme
      , normalBorderColor = th_normalBorder theme
      , focusFollowsMouse = False
      , keys = myKeys
      , terminal = "alacritty"
      , startupHook = do
          -- windows $ W.greedyView "work"
          configureXset
      , workspaces = myWorkspaces
      , handleEventHook =
          minimizeEventHook
            <> myServerModeEventHook
            <> handleEventHook desktopConfig
            <> Hacks.trayerAboveXmobarEventHook
      }
 where
  withSpacing size name x = named name (spacingWithEdge size x)
  tallLayout = withSpacing defSpacing "STall" (Tall 1 (10 / 100) (2 / 3))
  tallPinnedLayout = magicFocus $ withSpacing defSpacing "📌Tall" (Tall 1 (10 / 100) (2 / 3))
  myLayoutHook =
    avoidStruts $
      boringWindows $
        minimize $
          (lessBorders (Combine Union Never OnlyFloat)) $
            ( maximizeFocused (tallLayout ||| withSpacing defSpacing "SFull" Full)
                ||| (markEwmhFullscreen Full)
            )

-- \||| Full

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0"]

myManageHook :: ManageHook
myManageHook =
  composeAll . concat $
    [ [className =? b --> doF (W.shift "web") | b <- myClassWebShifts]
    , [resource =? c --> doF (W.shift "chat") | c <- myClassChatShifts]
    , [(appName =? "Alert" <&&> className =? "firefox") --> doFloat]
    , [className =? "xmonad-center-float" --> doCenterFloat]
    , [className =? i --> doFloat | i <- myClassFloats]
    , [isFullscreen --> (doF W.focusDown <+> doFullFloat)]
    , [(className =? "TeamViewer" <&&> stringProperty "WM_NAME" =? "") --> doIgnore]
    , [(className =? "xmonad-ignore") --> doIgnore]
    ]
 where
  myClassWebShifts = ["Navigator", "Firefox"]
  myClassChatShifts = ["Pidgin", "skype", "slack", "Telegram"]
  myClassFloats = ["Anki", "Gimp", "TeamViewer", "gtk-recordmydesktop", "Gtk-recordmydesktop", "xmonad-float"]
