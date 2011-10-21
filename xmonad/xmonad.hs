{-# OPTIONS_GHC -L/usr/lib #-}
module Main (main) where

import XMonad
-- import System.Exit
import Graphics.X11.Xlib
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.AppendFile
import XMonad.Util.XSelection
import XMonad.Actions.Search (google, isohunt, wayback, wikipedia, wiktionary, intelligent, selectSearch, promptSearch)
import XMonad.Actions.Commands
-- import XMonad.Prompt.RunOrRaise -- Doesn't seem to work right (sometimes freezes x11)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Spiral
import XMonad.Layout.HintedTile
import Data.Ratio ((%))
import XMonad.Layout.Grid
import XMonad.Layout.ComboP
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.Named
import XMonad.Layout.TwoPane
import XMonad.Layout.Combo
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WindowNavigation
--import XMonad.Layout.MagicFocus
import qualified XMonad.StackSet as W hiding (swapDown, swapUp)

import qualified Data.Map as M
import System.Exit


myModMask = mod4Mask
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#ff0000"
myBorderWidth = 2
myFocusFollowsMouse = False
myTerm = "urxvt -tint white -sh 18"
-- myTerm = "gnome-terminal --hide-menubar"
myHomedir = "/home/j/"

-- Workspaces
myWorkspaces =  map show [1..7] ++ ["8:comm", "9:web"]

myLayout = avoidStruts $ workspaceDir "~" (tiled) ||| (spiral (6/7)) ||| Full
tiled = named "HintedTall" $ hintedtile XMonad.Layout.HintedTile.Tall
  where 
    hintedtile = HintedTile nmaster delta ratio TopLeft
    nmaster = 1
    ratio = toRational (2/(1+sqrt 5 :: Double))
    delta = 0.03

tabConfig = defaultTheme { inactiveBorderColor = "#FF0000"
                         , activeTextColor     = "#00FF00"
                         }

-- myLayout = avoidStruts $ onWorkspace "9:web" browserLayout $ normalLayout
-- browserLayout = named "Tabbed" $ windowNavigation $ combined 
--   where
--     combined = combineTwo (TwoPane 0.03 0.5) (tabbed shrinkText tabConfig) (Mirror tiled)
-- normalLayout = tiled ||| (spiral (6/7)) ||| noBorders Full


main :: IO ()
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

myConfig = withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "l"] } $ defaultConfig {
         normalBorderColor  = myNormalBorderColor
        ,focusedBorderColor = myFocusedBorderColor
        ,keys               = myKeys
        ,modMask            = myModMask
        ,borderWidth        = myBorderWidth
        ,terminal           = myTerm
        ,XMonad.workspaces  = myWorkspaces
        ,layoutHook         = myLayout
        -- ,handleEventHook    = promoteWarp  # used with magic focus.
        ,focusFollowsMouse  = myFocusFollowsMouse
    }

myBar = "xmobar"

myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)


--newKeys x = M.union (keys defaultConfig x) (M.fromList (myKeys x))
--myKeys x = [ ((modMask x, xK_o), runOrRaisePrompt defaultXPConfig) ]


defKeys    = keys defaultConfig
delKeys x  = foldr M.delete           (defKeys x) (toRemove x)
myKeys x   = foldr (uncurry M.insert) (delKeys x) (toAdd    x)
 
-- modifier shortcuts
modm   = myModMask
shiftm = shiftMask
ctlm   = controlMask

-- remove some of the default key bindings
toRemove x = []
--     [ (modMask x              , xK_p  )
--     , (modMask .|.  shiftMask , xK_p)
--     ]

toAdd x  = 
     [ ((modm             , xK_p), prompt ("exec") defaultXPConfig)
       --((modm             , xK_p), runOrRaisePrompt defaultXPConfig)
     , ((modm .|. shiftm  , xK_p), prompt (myTerm ++ " -e ") defaultXPConfig)
     --, ((modm             , xK_x), xmonadPrompt defaultXPConfig)
     , ((modm .|. shiftm  , xK_x), changeDir defaultXPConfig)
     , ((modm .|. ctlm    , xK_n), appendFilePrompt defaultXPConfig $ myHomedir ++ ".notes/xmonad.txt")
     , ((modm             , xK_d), promptSearch greenXPConfig wikipedia)
     , ((modm .|. shiftm  , xK_d), selectSearch wikipedia)
     , ((modm             , xK_g), promptSearch greenXPConfig (intelligent google))
     , ((modm .|. shiftm  , xK_g), selectSearch (intelligent google))
     , ((modm             , xK_q), restart "xmonad" True)
     , ((modm .|. ctlm    , xK_q), io (exitWith ExitSuccess))
     , ((modm             , xK_x), spawn "xrandr --output VBOX0 --auto")
     ]

