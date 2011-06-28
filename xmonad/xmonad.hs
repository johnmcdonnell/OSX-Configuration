{-# OPTIONS_GHC -L/usr/lib #-}
module Main (main) where

import XMonad
-- import System.Exit
import Graphics.X11.Xlib
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Util.XSelection
import XMonad.Actions.Search (google, isohunt, wayback, wikipedia, wiktionary, intelligent, selectSearch, promptSearch)
-- import XMonad.Prompt.RunOrRaise -- Doesn't seem to work right (sometimes freezes x11)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spiral
import XMonad.Layout.HintedTile
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Roledex
--import XMonad.Layout.MagicFocus
import qualified XMonad.StackSet as W hiding (swapDown, swapUp)

import qualified Data.Map as M
import System.Exit


myModMask = mod1Mask               -- "command key (hopefully)"
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#ff0000"
myBorderWidth = 2
myLayout = avoidStruts $ workspaceDir "~" (spiral (6/7)) ||| Full ||| Roledex
myWorkspaces =  map show [1..7] ++ ["8:comm", "9:web"]
myFocusFollowsMouse = False
myTerm = "export COLORTERM=rxvt; urxvt"



--  colors match Ubuntu Human theme and Gnome panels
selected   = "'#0000ff'"
background = "'#efebe7'"
foreground = "'#000000'"


main :: IO ()
main = do
    xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

myConfig = defaultConfig {
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

--myDmenuTitleBar =
--    "exec `/usr/local/bin/dmenu_path | /usr/local/bin/dmenu\
--        \ -p 'Run:'\
--        \ -i\
--        \ -nb " ++ background ++ "\
--        \ -nf " ++ foreground ++ "\
--        \ -sb " ++ selected   ++ "\
--    \`"


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
     , ((modm .|. shiftm  , xK_p), prompt (myTerm ++ " --hidemenubar -x ") defaultXPConfig)
     , ((modm .|. shiftm  , xK_s), spawn ("exec " ++ myTerm ++ " -x ssh smash -Y"))
     , ((modm .|. shiftm  , xK_x), changeDir defaultXPConfig)
     , ((modm .|. ctlm    , xK_m), spawn ("exec midori --app=http://gmail.com"))
     , ((modm             , xK_c), promptSelection "pbcopy")  -- Copy to mac pasteboard.
     , ((modm             , xK_d), promptSearch greenXPConfig wikipedia)
     , ((modm .|. shiftm  , xK_d), selectSearch wikipedia)
     , ((modm             , xK_g), promptSearch greenXPConfig (intelligent google))
     , ((modm .|. shiftm  , xK_g), selectSearch (intelligent google))
     -- , ((modm             , xK_m), windows W.swapMaster)
     , ((modm             , xK_q), restart "xmonad" True)
     , ((modm .|. ctlm    , xK_q), io (exitWith ExitSuccess))
     ]

