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

-- Notifications using libnotify
instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]

-- XPConfig for Prompt
myXPConfig :: XPConfig
myXPConfig = def {font = "xft: Liberation Mono: size=7.5:bold:antialias=true", fgColor = "#fdf6e3", bgColor = "black", promptBorderWidth = 0, position = Top}


-- When compton or another window compositor is active, renders inactive frames transparent.
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
  xmproc <- spawnPipe "xmobar /home/sahiti/.xmobarcc" -- xmobar with config file
  spawn "pkill dunst ; dunst -config ~/.config/dunst/dunstrc &" -- notification display
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
       , focusedBorderColor	= "#586e75"} `additionalKeys` [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock") -- lockscreen
                                                              , ((mod4Mask , xK_b), sendMessage ToggleStruts) -- make xmobar invisible as needed
       	 			  	     		      , ((mod4Mask , xK_i), spawn "firefox") -- launch browser
       	 			  	     		      , ((mod4Mask , xK_e), spawn "emc") -- launch editor
       	 			  	     		      , ((mod4Mask , xK_v), spawn "pavucontrol") -- open volume control
       	 			  	     		      , ((mod4Mask , xK_p), spawn "dmenu_run") -- replace with promptShell eventually?
                                                              , ((mod4Mask,  xK_F5), spawn "~/emacs_capture \"org-protocol:/capture:/i/~\"") -- note capture
                                                              , ((mod4Mask .|. shiftMask,  xK_F5), spawn "~/emacs_capture \"org-protocol:/capture:/P/~\"") -- note capture selection
							      , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s ~/Screenshots/%Y-%m-%d-%T-screenshot.png") -- print screen after delay
                                                              , ((mod4Mask, xK_s), S.promptSearch myXPConfig S.multi) -- open search prompt
                                                              , ((mod4Mask .|. shiftMask, xK_s), S.selectSearch S.multi) -- search selection

							      , ((0, xK_Print), spawn "scrot") -- print screen
                                                              , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
                                                              , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
                                                              , ((0 , xF86XK_AudioRaiseVolume), spawn "pulseaudio-ctl up")
                                                              , ((0 , xF86XK_AudioLowerVolume), spawn "pulseaudio-ctl down")
                                                              , ((0 , xF86XK_AudioMute), spawn "pulseaudio-ctl mute")
                                                              ]
