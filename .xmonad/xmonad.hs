{-# LANGUAGE LambdaCase #-}

import Data.List (isPrefixOf)
import qualified Data.Map as M
import qualified Data.Text as T
import Graphics.X11.ExtraTypes.XF86
import System.Clipboard (getClipboardString)
import System.Directory (doesDirectoryExist, getHomeDirectory)
import System.FilePath ((</>))
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CycleWS
import XMonad.Actions.UpKeys
import XMonad.Actions.UpdatePointer
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
  ( runProcessWithInput,
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
      | "~/" `isPrefixOf` path = do
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
      (customFloating $ W.RationalRect 0 (6 / 10) 1 (4 / 10)),
    NS
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
        safeSpawn "alacritty" ["--working-directory", x, "--class", "scratchpad-quake"]
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

stopWhisper :: X ()
stopWhisper = do
  safeSpawn "/bin/bash" ["-c", "echo load ~/.config/numen/phrases/*.phrases| numenc"]
  safeSpawn "flatpak" ["run", "net.mkiol.SpeechNote", "--action", "stop-listening"]

_openEmacsAgenda :: X ()
_openEmacsAgenda = openInEmacs ["~/org/personal/gtd.org"]

openObsidian :: X ()
openObsidian =
  ifProcessRuns
    "obsidian"
    (return ())
    (safeSpawn "obsidian" ["obsidian://open?vault=share&file=Dashboard"])

toggleTouchpad :: X ()
toggleTouchpad = spawn "~/bin/toggleTouchpad"

toggleEarbuds :: X ()
toggleEarbuds = spawn "~/bin/toggleEarbuds"

muteSound :: X ()
muteSound = spawn "~/bin/mute"

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

-- sendClipboardToTelegram = spawn "~/bin/telegram-send"

-- * disable repeating (bouncing) ScrollLock key

-- * enable russian layout

--
-- Configuring X setting turned out to be complicated due to startup order. Some daemon overrides settings during user startup
-- and playing with systemctl startup sequence didn't help. As a workaround, this script has 5sec sleep and we run it here
-- asynchronously
configureXset :: X ()
configureXset = do spawnOnce "~/bin/configure-xset &"

-- mod1Mask - alt
-- mod4Mask - win
keysToAdd :: XConfig l -> [KeyBinding]
keysToAdd x =
  [ ((modMask x .|. shiftMask, xK_m), toggleTouchpad),
    ((modMask x .|. shiftMask, xK_b), toggleEarbuds),
    -- , ((modMask x, xK_c), toggleCapture)
    ((modMask x, xK_m), muteSound),
    ((modMask x, xK_s), stopWhisper),
    -- Move view to right or left workspace
    ((modMask x, xK_Left), prevWS),
    ((modMask x, xK_Right), nextWS),
    ((modMask x, xK_h), prevWS),
    ((modMask x, xK_l), nextWS),
    -- Move focused program to right or left workspace
    ((modMask x .|. shiftMask, xK_Left), shiftToPrev),
    ((modMask x .|. shiftMask, xK_Right), shiftToNext),
    ((modMask x .|. controlMask, xK_Return), safeSpawn "emacs" []),
    -- Mod + Tab enters "cycle through history" mode. Arrows to switch. Esc - exit.
    ((modMask x, xK_Tab), cycleRecentWS [xK_Tab] xK_Left xK_Right),
    -- Handle print screen using scrot utility. Resulting pictures are in in ~/Pictures
    ((controlMask, xK_Print), spawn "cd ~/Share; sleep 0.2; scrot -s"),
    ((0, xK_Print), spawn "cd ~/Share; scrot"),
    -- , (((modMask x), xK_F2), spawn "~/Downloads/NormCap-0.5.9-x86_64.AppImage -l chi --clipboard-handler xclip")

    -- Shortcuts to open programs
    ((modMask x, xK_F1), spawn "xprop | grep 'WM_CLASS\\|WM_NAME' | xmessage -file -"),
    -- , (((modMask x), xK_F2), safeSpawn "slack" [] >> safeSpawn "firefox" [])
    ((modMask x, xK_F3), openObsidian),
    ((modMask x, xK_F4), killOrSpawn "redshift" []),
    -- Toggle xmobar
    ((modMask x, xK_b), sendMessage ToggleStruts), -- Lock the screen
    ((modMask x, xK_z), do safeSpawn "xscreensaver-command" ["-lock"]),
    ((modMask x .|. shiftMask, xK_z), do spawn "sleep 1s; xset dpms force off"),
    -- Float and enlarge selected window
    ((modMask x, xK_f), withFocused (sendMessage . maximizeRestore)),
    -- resizing the master/slave ratio
    ((modm, xK_comma), sendMessage Shrink),
    ((modm, xK_period), sendMessage Expand),
    ((modm, xK_bracketleft), sendMessage (IncMasterN (-1))),
    ((modm, xK_bracketright), sendMessage (IncMasterN 1)),
    -- scratchpads
    ((modm, xK_Escape), quakeAct),
    ((modm, xK_grave), quakeAct),
    ((modm .|. shiftMask, xK_n), namedScratchpadAction scratchpads "numen"),
    -- See Graphics.X11.ExtraTypes.XF86
    ((0, xF86XK_AudioLowerVolume), spawn "~/bin/adjust-brightness -"),
    ((0, xF86XK_AudioRaiseVolume), spawn "~/bin/adjust-brightness +"),
    -- , ((0, xF86XK_AudioMute), spawn "sleep 0.5s; xset dpms force off")
    ((0, xF86XK_AudioMute), spawn "~/.xmonad/toggle-theme"),
    ((0, xK_Scroll_Lock), voxtypeStart), -- see myUpKeys for the stop action
    -- Pin(unpin) window to all workspaces
    ((modm .|. shiftMask, xK_a), windows copyToAll),
    ((modm, xK_a), killAllOtherCopies),
    ( (modm .|. shiftMask, xK_Return),
      do
        dir <- liftIO dirFromClipboard
        safeSpawn "alacritty" ["--working-directory", dir]
    )
  ]
    -- copy to workspace
    ++ [ ((m .|. modm, k), windows $ f i)
       | (i, k) <- zip (workspaces x) [xK_1 ..],
         (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]
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

-- Unused default key bindings
keysToRemove :: XConfig l -> [KeyCombination]
keysToRemove x =
  [ -- quake console
    (modMask x, xK_grave),
    -- temporarily mute sound
    (modMask x, xK_m),
    -- scratchpad numen
    (modMask x, xK_n),
    -- Xmobar is used as programs launcher
    (modMask x .|. shiftMask, xK_p),
    -- This one used for history cycle
    (modMask x, xK_Tab),
    -- These are remapped to < and >
    (modm, xK_h),
    (modm, xK_l),
    -- "<" and ">" are bound to shrink/expand master area
    (modm, xK_comma),
    (modm, xK_period),
    --
    (modm .|. shiftMask, xK_Return)
  ]
  where
    modm = modMask x

-- Modify default key bindings scheme
myKeys :: XConfig Layout -> M.Map KeyCombination (X ())
myKeys x = M.union (strippedKeys x) (M.fromList (keysToAdd x))
  where
    strippedKeys t = foldr M.delete (keys def t) (keysToRemove t)

mySB :: StatusBarConfig
mySB = statusBarProp "xmobar ~/.xmobarrc" (copiesPP (pad . xmobarColor "orange" "black") myPP)
  where
    myPP = xmobarPP
      { ppCurrent = xmobarColor "yellow" "" . wrap "[" "]",
        ppTitle = xmobarColor greenColor "" . shorten 50,
        ppHidden = noScratchPad
      }
    noScratchPad ws = if ws == "NSP" then "" else ws
    greenColor = "#8AE234"

main :: IO ()
main = xmonad
  . withEasySB mySB defToggleStrutsKey
  . (\c -> useUpKeys (def {grabKeys = True, upKeys = myUpKeys c}) c)
  . ewmhFullscreen
  . ewmh
  $ desktopConfig
    { manageHook = manageHook desktopConfig <+> namedScratchpadManageHook scratchpads <+> myManageHook,
      layoutHook = myLayoutHook,
      logHook =
        -- mouse pointer follows focus
        -- https://hackage.haskell.org/package/xmonad-contrib-0.18.1/docs/XMonad-Actions-UpdatePointer.html
        updatePointer (0.5, 0.5) (0, 0),
      modMask = mod4Mask,
      focusedBorderColor = redColor,
      focusFollowsMouse = False,
      keys = myKeys,
      terminal = "alacritty",
      startupHook = do
        windows $ W.greedyView "work"
        configureXset,
      workspaces = myWorkspaces
    }
  where
    myLayoutHook =
      maximize $ -- M-f to temporary maximize windows
        smartBorders $ -- Don't put borders on fullFloatWindows
          layoutHook desktopConfig
    redColor = "#Cd2626"

myWorkspaces :: [String]
myWorkspaces = ["web", "work", "3", "4", "5", "6", "7", "mail", "chat", "temp"]

myManageHook :: ManageHook
myManageHook =
  composeAll . concat $
    [ [className =? b --> doF (W.shift "web") | b <- myClassWebShifts],
      [resource =? c --> doF (W.shift "mail") | c <- myClassMailShifts],
      [resource =? c --> doF (W.shift "chat") | c <- myClassChatShifts],
      [(appName =? "Alert" <&&> className =? "firefox") --> doFloat],
      [className =? i --> doFloat | i <- myClassFloats],
      [(className =? "TeamViewer" <&&> stringProperty "WM_NAME" =? "") --> doIgnore],
      [isFullscreen --> (doF W.focusDown <+> doFullFloat)],
      [(className =? "ignore-window-manager") --> doIgnore]
    ]
  where
    myClassWebShifts = ["Navigator", "Firefox"]
    myClassMailShifts = ["Mail", "Thunderbird"]
    myClassChatShifts = ["Pidgin", "skype", "slack", "Telegram"]
    myClassFloats = ["Gimp", "TeamViewer", "gtk-recordmydesktop", "Gtk-recordmydesktop"]
