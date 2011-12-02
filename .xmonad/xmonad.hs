--
-- Xmonad configuration file
--   overrides some defaults and adds a few more functionalities

import XMonad
import XMonad.Core

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Man

import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.Grid
import Data.Ratio ((%))

import XMonad.Layout.Roledex

import XMonad.Layout.ComboP
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane


import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName

import XMonad.Util.EZConfig
import XMonad.Util.Run
import Graphics.X11.Xlib
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import System.IO

import XMonad.Actions.CycleWS


main = do
   myStatusBarPipe <- spawnPipe myStatusBar
   conkyRightBarPipe <- spawnPipe myConkyRightBar
   xmonad $ myUrgencyHook $ defaultConfig
      { terminal = "urxvt"
      , normalBorderColor  = myInactiveBorderColor
      , focusedBorderColor = myActiveBorderColor
      , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
      , layoutHook = myLayoutHook
      , startupHook = setWMName "LG3D"
      , logHook = dynamicLogWithPP $ myDzenPP myStatusBarPipe
      , modMask = mod1Mask
      , keys = myKeys
      , workspaces = myWorkspaces
     }   

-- Paths
myBitmapsPath = "/home/grender/.minimalDesktop/icons/"

-- Font
myFont = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*"

-- Colors
myBgBgColor = "black"
myFgColor = "gray80"
myBgColor = "gray20"
myHighlightedFgColor = "white"
myHighlightedBgColor = "gray40"

myActiveBorderColor = "gray80"
myInactiveBorderColor = "gray20"

myCurrentWsFgColor = "white"
myCurrentWsBgColor = "gray40"
myVisibleWsFgColor = "gray80"
myVisibleWsBgColor = "gray20"
myHiddenWsFgColor = "gray80"
myHiddenEmptyWsFgColor = "gray50"
myUrgentWsBgColor = "brown"
myTitleFgColor = "white"

myUrgencyHintFgColor = "white"
myUrgencyHintBgColor = "brown"

-- dzen general options
myDzenGenOpts = "-fg '" ++ myFgColor ++ "' -bg '" ++ myBgColor ++ "' -fn '" ++ myFont ++ "' -h '16'"

-- Status Bar
myStatusBar = "dzen2 -w 600 -ta l " ++ myDzenGenOpts

-- Conky Bar
myConkyRightBar = "conky -c /home/grender/.minimalDesktop/conky.conf | dzen2 -x 400 -w 800 -ta r " ++ myDzenGenOpts

-- 1200
-- 1280

myImLayout=(combineTwoP (TwoPane inc 0.2) rostersLayout simpleTabbed  roster) ||| Full
    where 
	rostersLayout= ResizableTall 2 0 0 []
	roster=pidginRoster `Or` trillianRoster
	pidginRoster=(ClassName "Pidgin") `And` (Role "buddy_list")
	trillianRoster= (Title "Trillian")
        inc=0.05

-- Layouts
myLayoutHook = avoidStruts $ smartBorders 
	    $ onWorkspace "im" myImLayout
--	    $ (tiled ||| Mirror tiled ||| Full)
	    $ (tiled ||| Full)
  where
    tiled = ResizableTall nmaster delta ratio []
    nmaster = 1
    delta = 1/100
    ratio = 3/5
-- Workspaces
-- myWorkspaces =
--   [
--      wrapBitmap "sm4tik/arch_10x10.xbm",
--      wrapBitmap "sm4tik/fox.xbm",
--      wrapBitmap "sm4tik/dish.xbm",
--      wrapBitmap "sm4tik/cat.xbm",
--      wrapBitmap "sm4tik/empty.xbm",
--      wrapBitmap "sm4tik/shroom.xbm",
--      wrapBitmap "sm4tik/bug_02.xbm",
--      wrapBitmap "sm4tik/eye_l.xbm",
--      wrapBitmap "sm4tik/eye_r.xbm"
--   ]
--   where
--      wrapBitmap bitmap = "^p(5)^i(" ++ myBitmapsPath ++ bitmap ++ ")^p(5)"

myWorkspaces    = ["web","dev","3","4","5","6","7","8","im"]

-- Urgency hint configuration
myUrgencyHook = withUrgencyHook dzenUrgencyHook
    {
      args = [
         "-x", "0", "-y", "568", "-h", "15", "-w", "1024",
         "-ta", "r",
         "-fg", "" ++ myUrgencyHintFgColor ++ "",
         "-bg", "" ++ myUrgencyHintBgColor ++ ""
         ]
    }

myManageHook = composeAll
             [ className =? "MPlayer"          --> doFloat
             , className =? "Vlc"              --> doFloat
             , className =? "Gimp"             --> doFloat
             , resource  =? "desktop_window"   --> doIgnore
             , resource  =? "kdesktop"         --> doIgnore 
             , className =? "Chromium-browser" --> moveTo "web"
             , className =? "Pidgin"            --> moveToAndGo "im"
             , appName   =? "trillian.exe"   --> moveToAndGo "im"
             ] 
                where 
                moveTo = doF . W.shift
                moveToAndGo ws = doF (W.greedyView ws) <+> doShift ws


-- Prompt config
myXPConfig = defaultXPConfig {
  position = Bottom,
  promptBorderWidth = 0,
  height = 15,
  bgColor = myBgColor,
  fgColor = myFgColor,
  fgHLight = myHighlightedFgColor,
  bgHLight = myHighlightedBgColor
  }

-- Union default and new key bindings
myKeys x  = M.union (M.fromList (newKeys x)) (keys defaultConfig x)

-- Add new and/or redefine key bindings
newKeys conf@(XConfig {XMonad.modMask = modm}) = [
  -- Use shellPrompt instead of default dmenu
  ((modm, xK_p), shellPrompt myXPConfig),
  -- Do not leave useless conky, dzen and xxkb after restart
  ((modm, xK_q), spawn "killall conky dzen2 xxkb; xmonad --recompile; xmonad --restart")
   , ((modm,               xK_Right),  nextWS)
   , ((modm,               xK_Left),    prevWS)
   , ((modm .|. shiftMask, xK_Right),  shiftToNext)
   , ((modm .|. shiftMask, xK_Left),    shiftToPrev)
   
    -- XF86AudioMute
    , ((0 , 0x1008ff12), spawn "amixer -q set Master toggle")
    -- XF86AudioLowerVolume
    , ((0 , 0x1008ff11), spawn "amixer -q set Master 2- unmute")
    -- XF86AudioRaiseVolume
    , ((0 , 0x1008ff13), spawn "amixer -q set Master 2+ unmute")   
   ]

-- Dzen config
myDzenPP h = defaultPP {
  ppOutput = hPutStrLn h,
  ppSep = "^bg(" ++ myBgBgColor ++ ")^r(1,15)^bg()",
  ppWsSep = " ",
  ppCurrent = wrapFgBg myCurrentWsFgColor myCurrentWsBgColor,
  ppVisible = wrapFgBg myVisibleWsFgColor myVisibleWsBgColor,
  ppHidden = wrapFg myHiddenWsFgColor,
  ppHiddenNoWindows = wrapFg myHiddenEmptyWsFgColor,
  ppUrgent = wrapBg myUrgentWsBgColor,
  ppTitle = (\x -> " " ++ wrapFg myTitleFgColor x),
  
  ppLayout  = dzenColor myFgColor"" .
                (\x -> case x of
                    "ResizableTall" -> wrapBitmap "dzen_bitmaps/tall.xbm"
                    "Mirror ResizableTall" -> wrapBitmap "dzen_bitmaps/mtall.xbm"
                    "Full" -> wrapBitmap "dzen_bitmaps/full.xbm"
                    otherwise -> wrapBitmap "dzen_bitmaps/tall.xbm"
                )
  }
  where
    wrapFgBg fgColor bgColor content= wrap ("^fg(" ++ fgColor ++ ")^bg(" ++ bgColor ++ ")") "^fg()^bg()" content
    wrapFg color content = wrap ("^fg(" ++ color ++ ")") "^fg()" content
    wrapBg color content = wrap ("^bg(" ++ color ++ ")") "^bg()" content
    wrapBitmap bitmap = "^p(5)^i(" ++ myBitmapsPath ++ bitmap ++ ")^p(5)"
