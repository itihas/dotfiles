--  -*- eval: (git-auto-commit-mode nil) -*-
import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.NamedWindows
import XMonad.Util.Run(spawnPipe, safeSpawn)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.NoBorders
import Data.List 
import System.IO
import Graphics.X11.ExtraTypes.XF86
import XMonad.Prompt
import qualified XMonad.Actions.Search as S
import qualified Data.Map as M
import XMonad.Prompt.Shell

import qualified XMonad.StackSet as W

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]

myXPConfig :: XPConfig
myXPConfig = def {font = "xft: Liberation Mono: size=7.5:bold:antialias=true", fgColor = "#fdf6e3", bgColor = "black", promptBorderWidth = 0, position = Top}


myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
	  where fadeAmount = 0.8

myManageHook :: ManageHook
myManageHook = composeAll . concat $
   [ [(resource =? "Dialog") --> doFloat]
     -- using list comprehensions and partial matches
   , [ className =?  c --> doFloat | c <- myFloatsC ]
   , [ fmap ( c `isInfixOf`) title     --> doFloat | c <- myMatchAnywhereFloatsT ]
   ]
   -- in a composeAll hook, you'd use: fmap ("VLC" `isInfixOf`) title --> doFloat
  where myFloatsC = ["Xmessage"]
        myMatchAnywhereFloatsC = ["Google","Pidgin"]
        myMatchAnywhereFloatsT = ["emacs-capture"] -- this one is silly for only one string!


main = do
  xmproc <- spawnPipe "xmobar /home/sahiti/.xmobarcc"
  spawn "pkill dunst ; dunst -config ~/.config/dunst/dunstrc &"
  xmonad $  ewmh
       $  withUrgencyHook LibNotifyUrgencyHook defaultConfig
       { workspaces = ["1","2","3","4","5","6","7","8","9","0","-","="]
       , manageHook = manageDocks <+> myManageHook <+> manageSpawn <+> manageHook defaultConfig
       , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook <+> docksEventHook
       , layoutHook =  smartBorders $ avoidStruts $ layoutHook defaultConfig
       , logHook = -- myLogHook <+>
         dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn xmproc
       	 	                   , ppCurrent = xmobarColor "#b58900" ""
                                   , ppTitle = xmobarColor "#b58900" "" . shorten 100
                                   , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip 
                                   }
       , borderWidth		= 0
       , modMask			= mod4Mask
       , terminal		= "xterm"
       , normalBorderColor	= "#002b36"
       , focusedBorderColor	= "#586e75"} `additionalKeys` [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
                                                              , ((mod4Mask , xK_b), sendMessage ToggleStruts)
       	 			  	     		      , ((mod4Mask , xK_i), spawn "chromium")
       	 			  	     		      , ((mod4Mask , xK_e), spawn "emc")
       	 			  	     		      , ((mod4Mask , xK_v), spawn "pavucontrol")
       	 			  	     		      , ((mod4Mask , xK_p), spawn "dmenu_run") -- replace with promptShell eventually?
                                                              , ((mod4Mask,  xK_F5), spawn "~/emacs_capture \"org-protocol:/capture:/i/~\"")
                                                              , ((mod4Mask .|. shiftMask,  xK_F5), spawn "~/emacs_capture \"org-protocol:/capture:/p/~\"")
							      , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s ~/Screenshots/%Y-%m-%d-%T-screenshot.png")
                                                              , ((mod4Mask, xK_s), S.promptSearchBrowser myXPConfig "/usr/bin/chromium" S.multi)
                                                              , ((mod4Mask .|. shiftMask, xK_s), S.selectSearchBrowser "/usr/bin/chromium" S.multi)

							      , ((0, xK_Print), spawn "scrot")
                                                              , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
                                                              , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
                                                              , ((0 , xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +1%")
                                                              , ((0 , xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -1%")
                                                              , ((0 , xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle")
                                                              ]
