import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Prompt.ConfirmPrompt
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (safeSpawn, spawnPipe)
import qualified XMonad.StackSet as W

import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO

-- Defaults
myModMask :: KeyMask
myModMask = mod4Mask -- set 'Mod' to Win key or Super_L

myTerminal :: String
myTerminal = "urxvtc" -- for Mod + Shift + Enter

myBorderWidth :: Dimension
myBorderWidth = 4

myNormalBorderColor :: String
myNormalBorderColor = "#2b303b"

myFocusedBorderColor :: String
myFocusedBorderColor = "#bf616a"

myWorkspaces :: [String]
myWorkspaces =
  [ "\xf0f4" -- 1 java
  , "\xf269" -- 2 www
  , "\xf044" -- 3 text
  , "\xf121" -- 4 code
  , "\xf0b1" -- 5 work
  , "\xf120" -- 6 ssh
  , "\xf109" -- 7 more work
  , "\xf0e0" -- 8 e-mail
  , "\xf27a" -- 9 chat
  , "\xf1bc" -- 0 music
  ]

-- Configuration
myConfig = def {
    modMask = myModMask
  , terminal = myTerminal
  , borderWidth = myBorderWidth
  , normalBorderColor = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , workspaces = myWorkspaces
  , manageHook = myManageHook
  , layoutHook = myLayoutHook
  , handleEventHook = myEventHook
  , focusFollowsMouse = False
  } `additionalKeys`
  [ ((myModMask, xK_p), safeSpawn "rofi" ["-combi-modi", "window,drun,run", "-show", "combi", "-modi", "combi", "-show-icons"])
  , ((myModMask .|. shiftMask, xK_q), confirmPrompt def "exit" $ io (exitWith ExitSuccess))
  , ((myModMask, xK_b), sendMessage ToggleStruts)
  , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
  , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
  , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  , ((0, xF86XK_MonBrightnessUp), spawn "lux -a 10%")
  , ((0, xF86XK_MonBrightnessDown), spawn "lux -s 10%")
  , ((myModMask, xK_f), spawn "thunar")
  , ((myModMask, xK_F1), spawn "nmcli radio wifi off")
  , ((myModMask, xK_F2), spawn "nmcli radio wifi on")
  , ((myModMask .|. shiftMask, xK_l), spawn "xtrlock -b")
  , ((myModMask, xK_0), windows $ W.greedyView "\xf1bc")
  , ((myModMask .|. shiftMask, xK_0), windows $ W.shift "\xf1bc")
  ]

-- hooks
--myManageHook = manageDocks <+> (isFullscreen --> doFullFloat) <+> manageHook def
myManageHook = manageDocks <+> manageHook def

-- myLayoutHook = smartBorders . avoidStruts $ (layoutHook def ||| Grid)
myLayoutHook =
  avoidStruts ( tall ||| Mirror tall ||| noBorders Full ||| Grid )
    where tall = Tall 1 ( 3 / 100 ) ( 1 / 2 )

myEventHook = docksEventHook <+> fullscreenEventHook

-- xmobar
myPP = xmobarPP {
    ppCurrent = xmobarColor "#bf616a" ""
  , ppHidden = xmobarColor "#c0c5ce" ""
  , ppHiddenNoWindows = xmobarColor "#4f5b66" ""
  , ppUrgent = xmobarColor "#a3be8c" ""
  , ppLayout = xmobarColor "#4f5b66" ""
  , ppTitle =  xmobarColor "#c0c5ce" "" . shorten 80
  , ppSep = xmobarColor "#4f5b66" "" "  "
}

-- TODO handle gracefully creating and destroying xmobar processes when monitor is connected/disconnected

main :: IO ()
main = do
  n <- countScreens
  xmprocs <- mapM (\i -> spawnPipe $ "xmobar ~/.xmonad/xmobarrc." ++ show i ++ " -x " ++ show i) [0..n-1]
  xmonad $ (ewmh myConfig) {
    logHook = mapM_ (\handle -> dynamicLogWithPP $ myPP { ppOutput = System.IO.hPutStrLn handle }) xmprocs
  }
