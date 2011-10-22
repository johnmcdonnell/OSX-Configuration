{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}
module Main (main) where

import Control.Monad

import XMonad
-- import System.Exit
import Graphics.X11.Xlib
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.AppendFile
import XMonad.Util.XSelection
import XMonad.Util.WindowProperties
import qualified XMonad.StackSet as S
import XMonad.Actions.Search (google, isohunt, wayback, wikipedia, wiktionary, intelligent, selectSearch, promptSearch)
import XMonad.Actions.Commands
-- import XMonad.Prompt.RunOrRaise -- Doesn't seem to work right (sometimes freezes x11)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Spiral
import XMonad.Layout.HintedTile
import XMonad.Layout.Grid
import Data.Ratio ((%))
import XMonad.Layout.ComboP
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.Named
import XMonad.Layout.TwoPane
import XMonad.Layout.Combo
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WindowNavigation
import XMonad.Layout.LayoutModifier
--import XMonad.Layout.MagicFocus
import qualified XMonad.StackSet as W hiding (swapDown, swapUp)

import qualified Data.Map as M
import System.Exit


myModMask = mod1Mask
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#ff0000"
myBorderWidth = 2
myFocusFollowsMouse = False
myTerm = "urxvt -tint white -sh 18"
-- myTerm = "gnome-terminal --hide-menubar"
myHomedir = "/home/j/"

-- Workspaces
myWorkspaces =  map show [1..7] ++ ["chat", "9:web"]


myLayout  = imLayout $ mainLayout


imLayout = onWorkspace "chat" $ avoidStruts $ withIMs ratio rosters chatLayout where
    chatLayout      = Grid
    ratio           = 1%6
    rosters         = [skypeRoster, pidginRoster]
    pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
    skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))


mainLayout = avoidStruts $ workspaceDir "~" (tiled) ||| (spiral 1.618) ||| Full
tiled = named "HintedTall" $ hintedtile XMonad.Layout.HintedTile.Tall
  where 
    hintedtile = HintedTile nmaster delta ratio TopLeft
    nmaster = 1
    ratio = toRational (2/(1+sqrt 5 :: Double))
    delta = 0.03

tabConfig = defaultTheme { inactiveBorderColor = "#FF0000"
                         , activeTextColor     = "#00FF00" }

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



 
-- modified version of XMonad.Layout.IM --
-- FROM
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Thomas_ten_Cate%27s_xmonad.hs
 
-- | Data type for LayoutModifier which converts given layout to IM-layout
-- (with dedicated space for the roster and original layout for chat windows)
data AddRosters a = AddRosters Rational [Property] deriving (Read, Show)
 
instance LayoutModifier AddRosters Window where
  modifyLayout (AddRosters ratio props) = applyIMs ratio props
  modifierDescription _                = "IMs"
 
-- | Modifier which converts given layout to IMs-layout (with dedicated
-- space for rosters and original layout for chat windows)
withIMs :: LayoutClass l a => Rational -> [Property] -> l a -> ModifiedLayout AddRosters l a
withIMs ratio props = ModifiedLayout $ AddRosters ratio props
 
-- | IM layout modifier applied to the Grid layout
gridIMs :: Rational -> [Property] -> ModifiedLayout AddRosters Grid a
gridIMs ratio props = withIMs ratio props Grid
 
hasAnyProperty :: [Property] -> Window -> X Bool
hasAnyProperty [] _ = return False
hasAnyProperty (p:ps) w = do
    b <- hasProperty p w
    if b then return True else hasAnyProperty ps w
 
-- | Internal function for placing the rosters specified by
-- the properties and running original layout for all chat windows
applyIMs :: (LayoutClass l Window) =>
               Rational
            -> [Property]
            -> S.Workspace WorkspaceId (l Window) Window
            -> Rectangle
            -> X ([(Window, Rectangle)], Maybe (l Window))
applyIMs ratio props wksp rect = do
    let stack = S.stack wksp
    let ws = S.integrate' $ stack
    rosters <- filterM (hasAnyProperty props) ws
    let n = fromIntegral $ length rosters
    let (rostersRect, chatsRect) = splitHorizontallyBy (n * ratio) rect
    let rosterRects = splitHorizontally n rostersRect
    let filteredStack = stack >>= S.filter (`notElem` rosters)
    (a,b) <- runLayout (wksp {S.stack = filteredStack}) chatsRect
    return (zip rosters rosterRects ++ a, b)
