{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, FlexibleContexts, NoMonomorphismRestriction #-}
module Main (main) where

import Control.Monad
import Control.Monad.Fix

import XMonad
-- import System.Exit
import Graphics.X11.Xlib
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Workspace
import XMonad.Util.XSelection
import XMonad.Util.WindowProperties
--import XMonad.Actions.Search (google, isohunt, wayback, wikipedia, wiktionary, intelligent, selectSearch, promptSearch)
import XMonad.Actions.TopicSpace
import XMonad.Actions.GridSelect
import XMonad.Actions.Commands
import XMonad.Actions.CycleWS
import XMonad.Actions.PhysicalScreens
-- import XMonad.Prompt.RunOrRaise -- Doesn't seem to work right (sometimes freezes x11)
import XMonad.ManageHook
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

import qualified XMonad.StackSet as W

import qualified Data.Map as M
import System.Exit


myModMask = mod1Mask
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#ff0000"
myBorderWidth = 2
myFocusFollowsMouse = False
myXPConfig = defaultXPConfig { font="-*-lucida-medium-r-*-*-14-*-*-*-*-*-*", height=22 }
myTerm = "urxvt -tint white -sh 18"
-- myTerm = "gnome-terminal --hide-menubar"
myShell = "zsh"
myHomedir = "/home/j/"
browserCmd = "firefox"
pdfViewer = "mupdf"

-- TODO: set this up.
--myLogHook = dynamicLogWithPP (myPrettyPrinter dbus)

--  Trying this topic thing out:
myTopics :: [Topic]
myTopics = 
    [ "dash"
    , "ws1", "ws2", "ws3", "irc", "talk", "admin", "xmonad", "pdf", "music", "web" ]

myTopicConfig :: TopicConfig
myTopicConfig =  defaultTopicConfig
    { topicDirs = M.fromList $
        [ ("admin",  "/etc")
        , ("xmonad",  myHomedir ++ ".xmonad")
        , ("admin",  "/etc")
        , ("pdf",  "/Documents")
        ]
    , defaultTopicAction = const $ spawnShell >*> 1
    , defaultTopic = "dashboard"
    , topicActions = M.fromList $ 
    [ ("admin", spawnCustomShell "su")
    , ("music",  openGoogleMusic >>
                spawn "spotify" >>
                spawn "gnome-alsamixer")
    , ("talk", spawn "pidgin")
    , ("irc", spawn "xchat")
    , ("web", spawn "firefox -browser")
    , ("xmonad", spawn "gvim /home/mcdon/.xmonad/xmonad.hs")
    --, ("vimrc", spawn "gvim /home/mcdon/.vimrc /home/mcdon/.vim")
    , ("pdf", spawn pdfViewer)
    ]
    }

openGoogleMusic = spawn  "firefox -new-window 'http://music.google.com'"

spawnCustomShell :: String -> X ()
spawnCustomShell shellname = currentTopicDir myTopicConfig  >>=  (spawnIn shellname)
    
spawnShell :: X ()
spawnShell =  currentTopicDir myTopicConfig  >>=  spawnShellIn

spawnShellIn :: Dir ->  X ()
spawnShellIn dir = spawnIn myShell dir

spawnIn :: String -> Dir -> X ()
spawnIn shellname dir = spawn $ myTerm ++ " '(cd ''" ++ dir ++ "'' && " ++ shellname ++ " )'"

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt myXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt myXPConfig $ windows . W.shift

-- Workspaces
myWorkspaces =  map show [1..7] ++ ["8:chat", "9:web"]


myLayout  = imLayout $ mainLayout


imLayout = onWorkspace "chat" $ avoidStruts $ withIMs ratio rosters chatLayout where
    chatLayout      = Grid
    ratio           = 1%6
    rosters         = [skypeRoster, pidginRoster]
    pidginRoster    = And (ClassName "Pidgin") (Role "buddy_list")
    skypeRoster     = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))


mainLayout = avoidStruts $ workspaceDir "~" (tiled) ||| (spiral (toRational (2/(1+sqrt 5 :: Double)))) ||| Full
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

-- | This is for gridselect
gsConfig = defaultGSConfig { gs_navigate = fix $ \self ->
    let navKeyMap = M.mapKeys ((,) 0) $ M.fromList $
                [(xK_Escape, cancel)
                ,(xK_Return, select)
                ,(xK_slash , substringSearch self)]
           ++
            map (\(k,a) -> (k,a >> self))
                [(xK_Left  , move (-1,0 ))
                ,(xK_h     , move (-1,0 ))
                ,(xK_n     , move (-1,0 ))
                ,(xK_Right , move (1,0  ))
                ,(xK_l     , move (1,0  ))
                ,(xK_i     , move (1,0  ))
                ,(xK_Down  , move (0,1  ))
                ,(xK_j     , move (0,1  ))
                ,(xK_e     , move (0,1  ))
                ,(xK_Up    , move (0,-1 ))
                ,(xK_u     , move (0,-1 ))
                ,(xK_y     , move (-1,-1))
                ,(xK_m     , move (1,-1 ))
                ,(xK_space , setPos (0,0))
                ]
    in makeXEventhandler $ shadowWithKeymap navKeyMap (const self) }

--main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig -- todo: restore
-- main = xmonad =<< (statusBar myBar myPP toggleStrutsKey myConfig)

main :: IO ()
main = do
    checkTopicConfig myTopics myTopicConfig
    let urgency = withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "l"] }
    -- Todo: put this back:
    --withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "l"] } $ 
    xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

myBar = "xmobar"
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
myConfig = defaultConfig {
         normalBorderColor  = myNormalBorderColor
        ,focusedBorderColor = myFocusedBorderColor
        ,keys               = myKeys
        ,modMask            = myModMask
        ,borderWidth        = myBorderWidth
        ,terminal           = myTerm
        --,XMonad.workspaces  = myWorkspaces
        ,workspaces         = myTopics
        ,layoutHook         = myLayout
        -- ,handleEventHook    = promoteWarp  # used with magic focus.
        ,focusFollowsMouse  = myFocusFollowsMouse
    }


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
toRemove x = 
     [ --(modMask x              , xK_p  )
       (modm .|. shiftm  , xK_q)   -- This is a little bit too easy to do accidentally.
     ]

toAdd x  = 
     [ ((modm             , xK_p), prompt ("exec") defaultXPConfig)
       --((modm             , xK_p), runOrRaisePrompt defaultXPConfig)
     , ((modm .|. shiftm  , xK_p), prompt (myTerm ++ " -e ") defaultXPConfig)
     --, ((modm             , xK_x), xmonadPrompt defaultXPConfig)
     -- Topics keymaps:
     , ((modm             , xK_n), spawnShell)
     , ((modm             , xK_a), currentTopicAction myTopicConfig)
     , ((modm             , xK_s), goToSelected defaultGSConfig)
     , ((modm             , xK_g), promptedGoto)
     , ((modm .|. shiftm  , xK_g), promptedShift)
     , ((modm             , xK_Tab), switchNthLastFocused myTopicConfig . succ . length . W.visible . windowset =<< get)
     
     , ((modm .|. shiftm  , xK_x), changeDir defaultXPConfig)
     , ((modm .|. ctlm    , xK_n), appendFilePrompt defaultXPConfig $ myHomedir ++ ".notes/xmonad.txt")
     --, ((modm             , xK_d), promptSearch greenXPConfig wikipedia)
     --, ((modm .|. shiftm  , xK_d), selectSearch wikipedia)
     --, ((modm             , xK_g), promptSearch greenXPConfig (intelligent google))
     --, ((modm .|. shiftm  , xK_g), selectSearch (intelligent google))
     , ((modm             , xK_q), restart "xmonad" True)
     , ((modm .|. ctlm    , xK_q), do
            spawn "kill $GNOME_KERYING_PID" 
            io (exitWith ExitSuccess)
            )
     , ((modm .|. shiftm  , xK_q), spawn "/opt/scripts/gothefucktosleep")
     , ((modm             , xK_z), toggleWS) -- from cycleWS
     -- , ((modm .|. shiftm  , xK_h), prevWS) -- from cycleWS
     -- , ((modm .|. shiftm  , xK_l), nextWS) -- from cycleWS
     ] ++ physicalScreenRemaps


physicalScreenRemaps = [((modm .|. mask, key), f sc)
     | (key, sc) <- zip [xK_w, xK_e, xK_r] [1,0,2]
     , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]


-- TODO: hook this up.
imManageHooks = composeAll [isIM --> moveToIM] where
    isIM     = foldr1 (<||>) [isPidgin, isSkype]
    isPidgin = className =? "Pidgin"
    isSkype  = className =? "Skype"
    moveToIM = doF $ W.shift "8:chat"
 
-- modified version of XMonad.Layout.IM --
-- FROM http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Thomas_ten_Cate%27s_xmonad.hs
 
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
            -> W.Workspace WorkspaceId (l Window) Window
            -> Rectangle
            -> X ([(Window, Rectangle)], Maybe (l Window))
applyIMs ratio props wksp rect = do
    let stack = W.stack wksp
    let ws = W.integrate' $ stack
    rosters <- filterM (hasAnyProperty props) ws
    let n = fromIntegral $ length rosters
    let (rostersRect, chatsRect) = splitHorizontallyBy (n * ratio) rect
    let rosterRects = splitHorizontally n rostersRect
    let filteredStack = stack >>= W.filter (`notElem` rosters)
    (a,b) <- runLayout (wksp {W.stack = filteredStack}) chatsRect
    return (zip rosters rosterRects ++ a, b)
