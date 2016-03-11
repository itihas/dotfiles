import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.NoBorders
import System.IO


main = do
     xmproc <- spawnPipe "xmobar /home/sahiti/.xmobarcc"
     xmonad $ defaultConfig { manageHook = manageDocks <+> manageHook defaultConfig
     	         	       , layoutHook =  smartBorders $ avoidStruts $ layoutHook defaultConfig
			       , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
			, ppCurrent = xmobarColor "#b58900" ""
                        , ppTitle = xmobarColor "#b58900" "" . shorten 100
                        }
        		       , borderWidth		= 0
			       , modMask			= mod4Mask
			       , terminal		= "xterm"
			       , normalBorderColor	= "#002b36"
			       , focusedBorderColor	= "#586e75"} `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
	, ((mod4Mask , xK_b), sendMessage ToggleStruts)
	, ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s ~/Screenshots/%Y-%m-%d-%T-screenshot.png")
        , ((0, xK_Print), spawn "scrot")
	]