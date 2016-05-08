--  -*- eval: (git-auto-commit-mode 1) -*-
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Run(spawnPipe, safeSpawn)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.NoBorders
import Data.List 
import System.IO

import qualified XMonad.StackSet as W

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
	  where fadeAmount = 0.8

myManageHook :: ManageHook
myManageHook = composeAll . concat $
   [ [ className =? "Firefox-bin" --> doShift "web" ]
   , [ className =? "Gajim.py"    --> doShift "jabber" ]
   , [(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]
 
     -- using list comprehensions and partial matches
   , [ className =?  c --> doFloat | c <- myFloatsC ]
   , [ fmap ( c `isInfixOf`) className --> doFloat | c <- myMatchAnywhereFloatsC ]
   , [ fmap ( c `isInfixOf`) title     --> doFloat | c <- myMatchAnywhereFloatsT ]
   ]
   -- in a composeAll hook, you'd use: fmap ("VLC" `isInfixOf`) title --> doFloat
  where myFloatsC = ["Gajim.py", "Xmessage"]
        myMatchAnywhereFloatsC = ["Google","Pidgin", "emacs-capture"]
        myMatchAnywhereFloatsT = ["emacs-capture"] -- this one is silly for only one string!

main = do
  xmproc <- spawnPipe "xmobar /home/sahiti/.xmobarcc"
  spawn "pkill dunst ; dunst -config ~/.config/dunst/dunstrc &"
  xmonad $  ewmh
       $  withUrgencyHook LibNotifyUrgencyHook defaultConfig { manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
       , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
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
							      , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s ~/Screenshots/%Y-%m-%d-%T-screenshot.png")
							      , ((0, xK_Print), spawn "scrot")
							      ]
