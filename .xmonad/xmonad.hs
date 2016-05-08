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
   [ [ className =? "Firefox-bin" --> doShift   "web" ]
   , [ className =? "Emacs"       --> doShift "emacs" ]
   , [(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]
 
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
       { workspaces = ["1:web","2:emacs","3:read","4:nav","5","6","7:music","8","9","0","-","="]
       , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
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
                                                              , ((xK_F5, xK_F5), spawn "exec ~/emacs_capture -e \'(org-capture nil \"i\")\'")
                                                              , ((mod4Mask .|. shiftMask, xK_F5), spawn "exec ~/emacs_capture -e \'(org-capture nil nil)\'")
							      , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s ~/Screenshots/%Y-%m-%d-%T-screenshot.png")
							      , ((0, xK_Print), spawn "scrot")
							      ]
