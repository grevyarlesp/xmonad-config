1c1,2
< -- xmonad config used by k4l1brx, modified from randomthought's config 
---
> -- xmonad config used by Malcolm MD
> -- https://github.com/randomthought/xmonad-config
3,4d3
< {-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable, MultiParamTypeClasses, NoMonomorphismRestriction, FlexibleContexts #-}
< -- ---------------------------------------------------------------------------
6,12d4
< 
< 
< import Data.Monoid
< import Data.Ratio ((%))
< import Data.Word
< import Control.Monad ((>=>), join, liftM, when, unless, forM)   -- For Custom Fullscreen Function
< import GHC.Word
13a6,7
> -- import System.Taffybar.Hooks.PagerHints (pagerHints)
> 
15,17c9,10
< import XMonad.Util.Font
< import XMonad.Layout.AvoidFloats
< import XMonad hiding ( (|||) )
---
> 
> import XMonad
19a13
> 
21d14
< import XMonad.Util.SpawnOnce 
24,38d16
< import XMonad.Hooks.ServerMode
< import qualified XMonad.Actions.FlexibleManipulate as Flex
< import qualified XMonad.Layout.BoringWindows as BR
< import XMonad.Layout.Decoration(Decoration, DefaultShrinker)
< import XMonad.Layout.LayoutModifier(LayoutModifier(handleMess, modifyLayout,
<                                     redoLayout),
<                                     ModifiedLayout(..))
< import XMonad.Layout.Simplest(Simplest(..))
< 
< import XMonad.Util.WorkspaceCompare
< import XMonad.Actions.CycleWS
< import XMonad.Layout.Reflect
< import XMonad.Layout.DecorationMadness
< import XMonad.Layout.BorderResize
< import XMonad.Layout.TrackFloating
41,43d18
< import XMonad.Hooks.Place
< import XMonad.Hooks.InsertPosition
< import qualified XMonad.Layout.Magnifier as Mag
45,46d19
< import XMonad.Layout hiding ( (|||) )
< import XMonad.Layout.TabBarDecoration
51,53d23
< import XMonad.Layout.IfMax
< 
< -- Layouts 
55,57d24
< import XMonad.Layout.SimplestFloat
< import XMonad.Layout.SimpleFloat
< import XMonad.Layout.WindowArranger
59,63d25
< import XMonad.Layout.LayoutCombinators
< import XMonad.Layout.ResizableTile -- for resizeable tall layout 
< import XMonad.Layout.MouseResizableTile -- for mouse control  
< --- Layout Modifiers 
< import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
66d27
< import qualified XMonad.Layout.ToggleLayouts as TL
70c31
< 
---
> import XMonad.Layout.Simplest
73d33
< 
75c35
< import XMonad.Layout.Grid
---
> 
84,92d43
< import XMonad.Actions.FloatKeys
< -- Tree Select 
< import Data.Tree
< import qualified XMonad.Actions.TreeSelect as TS
< import XMonad.Hooks.WorkspaceHistory
< import qualified XMonad.StackSet as W
< 
< import XMonad.Actions.FloatSnap
< import XMonad.Actions.Promote
94,115d44
< import XMonad.Prompt
< import XMonad.Prompt.FuzzyMatch
< import XMonad.Prompt.Theme
< 
< import XMonad.Util.NamedScratchpad
< import XMonad.Prompt.Shell 
< import XMonad.Actions.CopyWindow
< import XMonad.Actions.GridSelect
< import XMonad.Util.WindowProperties
< 
< 
< -----------------------------------Custom Layout---------------------------
< import XMonad.Util.NamedWindows ( getName )
< import Data.Traversable ( traverse )
< import Data.Maybe ( maybeToList )
< import Data.List ( (\\) )
< 
< logTitles :: X (Maybe String) -- this is a Logger
< logTitles = withWindowSet $ fmap (Just . unwords) -- fuse window names
<                           . traverse (fmap show . getName) -- show window names
<                           . (\ws -> W.index ws \\ maybeToList (W.peek ws))
<                           -- all windows except the focused (may be slow)
121,124c50
< 
< -- myTerminal = "~/.scripts/launch_kitty.sh"
< myTerminalDir = "alacritty --working-directory `xcwd`"
< myTerminal = "alacritty"
---
> myTerminal = "termite"
127c53
< myScreensaver = "slock"
---
> myScreensaver = "dm-tool switch-to-greeter"
131c57
< mySelectScreenshotCliboard = "flameshot gui"
---
> mySelectScreenshot = "select-screenshot"
133d58
< mySelectScreenshot = "flameshot gui"
135c60,61
< myScreenshot = "flameshot gui"
---
> myScreenshot = "xfce4-screenshooter"
> 
138c64,65
< myLauncher = "j4-dmenu-desktop"
---
> myLauncher = "rofi -show"
> 
141d67
< -----------------------------------------------------------------------
145a72,73
> myWorkspaces = ["1: term","2: web","3: code","4: media"] ++ map show [5..9]
> 
147,161d74
< xmobarEscape :: String -> String
< xmobarEscape = concatMap doubleLts
<   where
<         doubleLts '<' = "<<"
<         doubleLts x   = [x]
< 
< myWS = ["1 \xfbe2  ", "2 \xf269  ", "3 \xf0c3 ", "4 \xe62b ","5 \xe62b ", "6 \xf016  ", "7 \xf044 ", "8 \xf126","9 \xf152  "]
< 
< myWorkspaces :: [String]
< myWorkspaces = clickable . map xmobarEscape
<                $ ["1 \xfbe2  ", "2 \xf269  ", "3 \xf0c3 ", "4 \xe62b ","5 \xe62b ", "6 \xf016  ", "7 \xf044 ", "8 \xf126","9 \xf152  "]
<   where
<         clickable l = [ "<action=xdotool key super+" ++ show n ++ ">" ++ ws ++ "</action>" |
<                       (i,ws) <- zip [1..9] l,
<                       let n = i ]
176c89,107
< ------------------------
---
> myManageHook = composeAll
>     [
>       className =? "Google-chrome"                --> doShift "2:web"
>     , resource  =? "desktop_window"               --> doIgnore
>     , className =? "Galculator"                   --> doCenterFloat
>     , className =? "Steam"                        --> doCenterFloat
>     , className =? "Gimp"                         --> doCenterFloat
>     , resource  =? "gpicview"                     --> doCenterFloat
>     , className =? "MPlayer"                      --> doCenterFloat
>     , className =? "Pavucontrol"                  --> doCenterFloat
>     , className =? "Mate-power-preferences"       --> doCenterFloat
>     , className =? "Xfce4-power-manager-settings" --> doCenterFloat
>     , className =? "VirtualBox"                   --> doShift "4:vm"
>     , className =? "Xchat"                        --> doShift "5:media"
>     , className =? "stalonetray"                  --> doIgnore
>     , isFullscreen                                --> (doF W.focusDown <+> doFullFloat)
>     -- , isFullscreen                             --> doFullFloat
>     ]
> 
178,208d108
< -- Android studio fix 
< (~=?) :: Eq a => Query [a] -> [a] -> Query Bool
< q ~=? x = fmap (L.isInfixOf x) q
< -- Do not treat menus and settings popup as a separate window.
< manageIdeaCompletionWindow = (className =? "jetbrains-studio") <&&> (title ~=? "win") --> doIgnore
< 
< -- $ ["1 \xfbe2  ", "2 \xf269  ", "3 \xf0c3 ", "4 \xe62b ","5 \xe62b ", "6 \xf016  ", "7 \xf044 ", "8 \xf126","9 \xf152  "]
< myManageHook = 
<       (isDialog --> doF W.swapUp)                       -- Bring Dialog Window on Top of Parent Floating Window
<        <+> insertPosition Below Newer                    -- Insert New Windows at the Bottom of Stack Area
<        <+> manageIdeaCompletionWindow                    -- Adding Fix for Android Studio
<        <+>
<       composeAll
<       [
<       className =? "discord"                --> doShift ( myWorkspaces !! 0 )
<       -- , className =? "okular"                --> doShift $ 
<       , className =? "knotes"                --> doIgnore
<       , resource  =? "desktop_window"               --> doIgnore
<       , className =? "Galculator"                   --> doCenterFloat
<       , className =? "Steam"                        --> doCenterFloat
<       , className =? "popup-bottom-center"          --> doCenterFloat
<       , className =? "Gimp"                         --> doCenterFloat
<       , resource  =? "gpicview"                     --> doCenterFloat
<       , className =? "MPlayer"                      --> doCenterFloat
<       , className =? "Pavucontrol"                  --> doCenterFloat
<       , className =? "Xfce4-power-manager-settings" --> doCenterFloat
<       , className =? "Xchat"                        --> doShift "5:media"
<       , className =? "stalonetray"                  --> doIgnore
<       -- , isFullscreen                                --> (doF W.focusDown <+> doFullFloat)
<       , isFullscreen                             --> doFullFloat
<       ]
220,221d119
< mySubTabbed  x = addTabs shrinkText myTabTheme $ subLayout [] Simplest x
< 
225,285c123,141
< tab          = avoidStruts
<               $ renamed [Replace "[T]"]
<                $ tabbedBottomAlways shrinkText myTabTheme
< 
< myTall = renamed [Replace "[]="]
<           $ mySubTabbed $ BR.boringWindows 
<           $ windowNavigation
<           -- $ addTopBar
<           $ addSpace
<           $ windowArrange 
<           -- $ tabBar shrinkText myTabTheme Bottom (gaps[(D, 18)] $ (Tall 1 (3/100) (1/2)))
<           $ ResizableTall 1 (3/100) (1/2) []
< 
< myMagnifyTall = renamed [Replace "[MT]"]
<           -- $ addTopBar
<           $ addSpace
<           $ windowArrange 
<           -- $ tabBar shrinkText myTabTheme Bottom (gaps[(D, 18)] $ (Tall 1 (3/100) (1/2)))
<           -- $ tabBar shrinkText myTabTheme Bottom (gaps[(D, 16)] $ (Mag.magnifier(ResizableTall 1 (3/100) (1/2) [])))
<           $ Mag.magnifier(ResizableTall 1 (3/100) (1/2) [])
< 
< myMagnifyGrid = renamed [Replace "[MG]"]
<       -- $ addTopBar
<       $ windowNavigation
<       $ addSpace
<       $ Mag.magnifier $ Grid
< 
< my3C = renamed [Replace "[=]"]
<       $ mySubTabbed $ BR.boringWindows 
<       $ windowNavigation
<       $ addSpace
<       $ ThreeCol 1 (3/100) (1/2)
< 
< myGrid = renamed [Replace "[G]"]
<       $ mySubTabbed $ BR.boringWindows 
<       $ windowNavigation
<       $ addSpace
<       $ Grid
< 
< myRez = renamed [Replace "[S]"]
<       $ windowNavigation
<       $ simplestFloat
< 
< -- layouts      = TL.toggleLayouts (avoidStruts myRez) (windowArrange (tab ||| avoidStruts (
< layouts      = TL.toggleLayouts (avoidStruts myRez) (windowArrange (tab ||| avoidStruts (
<                       myMagnifyGrid ||| myMagnifyTall ||| myTall ||| my3C ||| myGrid
<                   )))
< 
< myLayoutSelect = [
<          ("[T]", sendMessage $ JumpToLayout "[T]")
<          ,("[]=", sendMessage $ JumpToLayout "[]=")
<          ,("[G]",    sendMessage $ JumpToLayout "[G]")
<          ,("[=]",    sendMessage $ JumpToLayout "[=]")
<          ]
< 
< selectLayout     = runSelectedAction (myGSConfig $ myColor color4) myLayoutSelect
< 
< myGSConfig colorizer = (buildDefaultGSConfig colorizer)
<   {gs_cellheight  = 50
<   ,gs_cellwidth   = 125
<   }
---
> tab          =  avoidStruts
>                $ renamed [Replace "Tabbed"]
>                $ addTopBar
>                $ myGaps
>                $ tabbed shrinkText myTabTheme
> 
> layouts      = avoidStruts (
>                 (
>                     renamed [CutWordsLeft 1]
>                   $ addTopBar
>                   $ windowNavigation
>                   $ renamed [Replace "BSP"]
>                   $ addTabs shrinkText myTabTheme
>                   $ subLayout [] Simplest
>                   $ myGaps
>                   $ addSpace (BSP.emptyBSP)
>                 )
>                 ||| tab
>                )
291,297d146
< 
< -- Colorizer generator
< myColor color _ isFg = do
<   return $ if isFg
<            then (color, color0)
<            else (color0 ,color)
< 
311,399d159
< -------
< -- Tree Menu
< treeselectAction :: TS.TSConfig (X ()) -> X ()
< treeselectAction myMenu = TS.treeselectAction myMenu
<   [
<     Node (TS.TSNode "Session" "Session" (return())) 
<      [
<        Node(TS.TSNode "Lock" "Lock the system" (spawn myScreensaver)) []
<        , Node(TS.TSNode "Suspend" "Suspend" (spawn "systemctl suspend")) []
<        , Node(TS.TSNode "Logout" "logout" (io (exitWith ExitSuccess))) []
<        , Node(TS.TSNode "Reboot" "Reboot the system" (spawn "systemctl reboot")) []
<        , Node(TS.TSNode "Shutdown" "Poweroff the system" (spawn "systemctl poweroff")) []
<      ]
<    , Node (TS.TSNode "Brightness" "Sets screen brightness using xbacklight" (return ()))
<        [ Node (TS.TSNode "100%" "FULL POWER!!"            (spawn "xbacklight -set 100")) []
<        , Node (TS.TSNode "75%" "Normal Brightness (75%)" (spawn "xbacklight -set 75"))  []
<        , Node (TS.TSNode "50%" "Normal Brightness (50%)" (spawn "xbacklight -set 50"))  []
<        , Node (TS.TSNode "25%"    "Quite dark"              (spawn "xbacklight -set 25"))  []
<        ]
<    ]
< tsDefaultConfig :: TS.TSConfig myMenu
< tsDefaultConfig = TS.TSConfig { 
<                              TS.ts_hidechildren = True
<                            , TS.ts_background   = 0xc083a598
<                            , TS.ts_font         = "xft:Sans-16"
<                            , TS.ts_node         = (0xff1d2021, 0xff83a598)
<                            , TS.ts_nodealt      = (0xff1d2021, 0xff85b6a3)
<                            , TS.ts_highlight    = (0xffffffff, 0xffb8bb26)
<                            , TS.ts_extra        = 0xffb8bb26
<                            , TS.ts_node_width   = 200
<                            , TS.ts_node_height  = 30
<                            , TS.ts_originX      = 20
<                            , TS.ts_originY      = 20
<                            , TS.ts_indent       = 80
<                            , TS.ts_navigate     = myTreeNavigation
<                            }
< myTreeNavigation = M.fromList
<     [ ((0, xK_Escape),   TS.cancel)
<     , ((0, xK_Return),   TS.select)
<     , ((0, xK_space),    TS.select)
<     , ((0, xK_Up),       TS.movePrev)
<     , ((0, xK_Down),     TS.moveNext)
<     , ((0, xK_Left),     TS.moveParent)
<     , ((0, xK_Right),    TS.moveChild)
<     , ((0, xK_k),        TS.movePrev)
<     , ((0, xK_j),        TS.moveNext)
<     , ((0, xK_h),        TS.moveParent)
<     , ((0, xK_l),        TS.moveChild)
<     , ((0, xK_o),        TS.moveHistBack)
<     , ((0, xK_i),        TS.moveHistForward)
<     , ((0, xK_d),        TS.moveTo ["dev"])
<     , ((0, xK_g),        TS.moveTo ["graphics"])
<     , ((0, xK_m),        TS.moveTo ["music"])
<     , ((0, xK_v),        TS.moveTo ["video"])
<     , ((0, xK_w),        TS.moveTo ["web"])
<     , ((mod4Mask, xK_b), TS.moveTo ["web", "browser"])
<     , ((mod4Mask, xK_c), TS.moveTo ["web", "chat"])
<     , ((mod4Mask, xK_m), TS.moveTo ["web", "email"])
<     , ((mod4Mask, xK_r), TS.moveTo ["web", "rss"])
<     , ((mod4Mask, xK_w), TS.moveTo ["web", "web conference"])
<     , ((mod4Mask, xK_d), TS.moveTo ["dev", "docs"])
<     , ((mod4Mask, xK_e), TS.moveTo ["dev", "emacs"])
<     , ((mod4Mask, xK_f), TS.moveTo ["dev", "files"])
<     , ((mod4Mask, xK_p), TS.moveTo ["dev", "programming"])
<     , ((mod4Mask, xK_t), TS.moveTo ["dev", "terminal"])
<     , ((mod4Mask, xK_z), TS.moveTo ["dev", "virtualization"])
<     , ((mod4Mask, xK_g), TS.moveTo ["graphics", "gimp"])
<     , ((mod4Mask, xK_i), TS.moveTo ["graphics", "image viewer"])
<     , ((mod4Mask, xK_u), TS.moveTo ["music", "music player"])
<     , ((mod4Mask, xK_o), TS.moveTo ["video", "obs"])
<     , ((mod4Mask, xK_v), TS.moveTo ["video", "video player"])
<     , ((mod4Mask, xK_k), TS.moveTo ["video", "kdenlive"])
<     , ((mod4Mask .|. altMask, xK_h), TS.moveTo ["dev", "programming", "haskell"])
<     , ((mod4Mask .|. altMask, xK_p), TS.moveTo ["dev", "programming", "python"])
<     , ((mod4Mask .|. altMask, xK_s), TS.moveTo ["dev", "programming", "shell"])
<     ]
< 
< 
< -- getSortByIndexNoSP =
< --         fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
< 
< windowsNoSP :: (WindowSet -> WindowSet) -> X ()
< windowsNoSP = windows
< 
< 
< myGoToSelected :: GSConfig Window -> X ()
< myGoToSelected = withSelectedWindow $ windowsNoSP . W.focusWindow
< 
< gsconfig2 colorizer = (buildDefaultGSConfig colorizer) { gs_cellheight = 50, gs_cellwidth = 200 }
406,408c166
< xmobarTitleColor = background
< xmobarCurrentBackground = color4
< xmobarCurrentForeground = background
---
> xmobarTitleColor = "#C678DD"
416,417c174,175
< myNormalBorderColor     = color8
< myFocusedBorderColor    = color4
---
> myNormalBorderColor     = "#000000"
> myFocusedBorderColor    = active
428a187
> red     = "#dc322f"
433,460c192
< 
< foreground   = "#d5c4a1"
< background   = "#1d2021"
< cursor       = "#d5c4a1"
< 
< color0       = "#1d2021"
< color8       = "#665c54"
< 
< color1       = "#fb4934"
< color9       = "#cc241d"
< 
< color2       = "#b8bb26"
< color10      = "#b8bb26"
< 
< color3       = "#fabd2f"
< color11      = "#fabd2f"
< 
< color12       = "#458588"
< color4      = "#83a598"
< 
< color5       = "#d3869b"
< color13      = "#d3869b"
< 
< color6       = "#8ec07c"
< color14      = "#8ec07c"
< 
< color7       = "#d5c4a1"
< color15      = "#fbf1c7"
---
> green   = "#859900"
470c202
< activeWarn  = color1
---
> activeWarn  = red
477,484c209,211
< -- myFont      = "xft:Inconsolata Nerd Font:size=9:bold:antialias=true"
< -- myBigFont   = "xft:Inconsolata Nerd Font:size=9:bold:antialias=true"
< -- myWideFont  = "xft:Inconsolata Nerd Font:"
< --             ++ "style=Regular:pixelsize=180:hinting=true"
< 
< myFont      = "Utf8:JetBrains Mono:size=9:bold:antialias=true"
< myBigFont   = "Utf8:JetBrains Mono:size=9:bold:antialias=true"
< myWideFont  = "Utf8:JetBrains Mono:"
---
> myFont      = "xft:Zekton:size=9:bold:antialias=true"
> myBigFont   = "xft:Zekton:size=9:bold:antialias=true"
> myWideFont  = "xft:Eurostar Black Extended:"
485a213
> 
491,498c219,226
<     , activeColor           = color4
<     , inactiveColor         = color8
<     , activeBorderColor     = color4
<     , inactiveBorderColor   = color8
<     , activeTextColor       = color4
<     , inactiveTextColor     = color8
<     , urgentBorderColor     = color1
<     , urgentTextColor       = color1
---
>     , inactiveBorderColor   = base03
>     , inactiveColor         = base03
>     , inactiveTextColor     = base03
>     , activeBorderColor     = active
>     , activeColor           = active
>     , activeTextColor       = active
>     , urgentBorderColor     = red
>     , urgentTextColor       = yellow
505,513c233,239
<     -- { fontName = "xft:LiberationSans-Bold:size=9:antialias=true,ipamincho:size=10"
<     { fontName = "xft:Sarasa Gothic J:size=9:antialias=true"
<     , activeColor           = color4
<     , inactiveColor         = color8
<     , activeBorderColor     = color4
<     , inactiveBorderColor   = color8
<     , activeTextColor       = background
<     , inactiveTextColor     = foreground
<     , decoHeight            = 16
---
>     { fontName              = myFont
>     , activeColor           = active
>     , inactiveColor         = base02
>     , activeBorderColor     = active
>     , inactiveBorderColor   = base02
>     , activeTextColor       = base03
>     , inactiveTextColor     = base00
516,544d241
< --
< 
< scratchpads = [
<   NS "todo" spawnTerm findTerm manageTerm,
<   NS "gen" spawnGen findGen manageTerm,
<   NS "wiki" spawnWiki findWiki nonFloating
<               ]
<   where 
<     role = stringProperty "WM_WINDOW_ROLE"
<     -- spawnTerm = "GLFW_IM_MODULE=ibus kitty --name scratchpad --session ~/.config/kitty/todo.conf"
<     -- spawnWiki = "GLFW_IM_MODULE=ibus kitty --name scratchpad_wiki --session ~/.config/kitty/vimwiki.conf"
<     -- spawnGen = "GLFW_IM_MODULE=ibus kitty --name scratchpad_gen --title Scratchpad"
<     spawnTerm = "st -c scratchpad -t Todo -e nvim ~/vimwiki/Reminder.wiki"
<     spawnWiki = "st -c scratchpad_wiki -t Wiki -e nvim -c VimwikiIndex"
<     spawnGen = "st -c scratchpad_gen -t Scratchpad"
< 
<     findTerm = resource =? "scratchpad"
<     findGen = resource =? "scratchpad_gen"
<     findWiki = resource =? "scratchpad_wiki"
<     manageTerm = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below
<       where
<         -- reusing these variables is ok since they're confined to their own 
<         -- where clauses 
<         h = (1/2)       -- height, 10% 
<         w = (1/2)       -- width, 100%
<         t = 0.1            -- bottom edge
<         l = (1 - w)/2-- centered left/right
< 
< 
553d249
< 
557d252
< 
562,589d256
<   [
<   ((modMask .|. controlMask .|. shiftMask, xK_k),
<   spawn "xkill")
<   , ((modMask .|. controlMask, xK_space),
<   selectLayout)
<   ]
<   ++
<   [
<   ((modMask, xK_bracketright),
<      spawn "~/.xmonad/pomobar.sh")
<   , ((modMask .|. shiftMask, xK_bracketright),
<      spawn "~/.xmonad/pomobar_stop.sh")
<   ]
<   ++
<   [
<    ((modMask, xK_v ), windows copyToAll) -- @@ Make focused window always visible
<  , ((modMask .|. shiftMask, xK_v ),  killAllOtherCopies) -- @@ Toggle window state back
<  , ((modMask .|. shiftMask, xK_c     ), kill1) -- @@ Close the focused window
<   ]
<   ++
<   [
<    ((modMask, xK_bracketleft), myGoToSelected $ gsconfig2 defaultColorizer)
<    , ((modMask, xK_F8), spawn "~/.scripts/displayselect")
<   ]
<   ++
<   [ ((modMask, xK_d),
<      treeselectAction tsDefaultConfig)
<   -- Start a terminal.  Terminal to start is specified by myTerminal variable.
591c258,259
<    , ((modMask .|. altMask, xK_Return),
---
>   -- Start a terminal.  Terminal to start is specified by myTerminal variable.
>   [ ((modMask .|. shiftMask, xK_Return),
595c263
<   , ((controlMask .|. altMask, xK_l),
---
>   , ((modMask, xK_0),
603,610d270
<   , ((modMask .|. shiftMask, xK_Return),
<       spawn "pcmanfm-qt \"`xcwd`\"")
<   -- Take a full screenshot using the command specified by myScreenshot.
<   , ((modMask, xK_Print),
<      spawn myScreenshot)
< 
<   , ((modMask .|. shiftMask .|. controlMask, xK_F8),
<     spawn "~/.scripts/dmenurecord")
612c272
<   , ((modMask .|. shiftMask, xK_Print),
---
>   , ((modMask .|. shiftMask, xK_p),
615,616c275,277
<   -- , ((modMask, xK_space),
<      -- spawn "~/.scripts/Toggle_Ibus.sh")
---
>   -- Take a full screenshot using the command specified by myScreenshot.
>   , ((modMask .|. controlMask .|. shiftMask, xK_p),
>      spawn myScreenshot)
620,621d280
<   , ((modMask, xK_s), sendMessage $ TL.Toggle "[S]")
<   , ((modMask .|. shiftMask, xK_p), sendMessage $ JumpToLayout "[T]")
625c284
<      spawn "~/.scripts/volume_change.sh toggle")
---
>      spawn "amixer -q set Master toggle")
629c288
<      spawn "~/.scripts/volume_change.sh 4%-")
---
>      spawn "amixer -q set Master 5%-")
633c292
<      spawn "~/.scripts/volume_change.sh 4%+")
---
>      spawn "amixer -q set Master 5%+")
635,639d293
<   , ((0, xF86XK_MonBrightnessUp),
<       spawn "~/.scripts/brightness_change.sh +")
<           
<   , ((0, xF86XK_MonBrightnessDown),
<       spawn "~/.scripts/brightness_change.sh -")
661c315
<   , ((modMask .|. shiftMask, xK_w),
---
>   , ((modMask .|. shiftMask, xK_c),
665c319
<   , ((modMask .|. altMask, xK_space),
---
>   , ((modMask, xK_space),
678a333
> 
681,686c336
<      windows W.focusUp)
<   , ((modMask .|. altMask, xK_j),
<     BR.focusDown)
< 
<   , ((modMask .|. altMask, xK_k),
<     BR.focusUp)
---
>      windows W.focusUp  )
702c352
<      windows W.swapUp)
---
>      windows W.swapUp    )
711,717d360
<   -- , ((modMask, xK_a), sendMessage ShrinkSlave) -- %! Shrink a slave area
<   -- , ((modMask, xK_z), sendMessage ExpandSlave) -- %! Expand a slave area
<   --
<   , ((modMask, xK_z), incWindowSpacing 2) -- %! Inc Gap
<   , ((modMask, xK_x), decWindowSpacing 2) -- %! Dec Gap
<   , ((modMask .|. shiftMask, xK_a), setScreenWindowSpacing 0) -- %! Inc Gap
<   , ((modMask , xK_a), toggleWindowSpacingEnabled) -- %! Inc Gap
735c378
<   , ((modMask .|. altMask, xK_q),
---
>   , ((modMask .|. shiftMask, xK_q),
739c382
<   , ((modMask .|. altMask, xK_r),
---
>   , ((modMask, xK_q),
746d388
<   -- mod-control-shift-[1..9] @@ Copy client to workspace N
749c391
<       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]
---
>       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
754,757c396,401
<   -- [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
<   --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
<   --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
<   -- ++
---
>   [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
>       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
>       , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
> 
> 
>   ++
778d421
<   ++
780,783d422
<   [
<     ((modMask, xK_o), shellPrompt myXPromptConfig)
<     , ((modMask .|. shiftMask, xK_o), spawn "~/.scripts/mpdmenu")
<   ]
784a424,425
>   -- Some bindings for BinarySpacePartition
>   -- https://github.com/benweitzman/BinarySpacePartition
786,787c427,439
<    ((modMask .|. controlMask              , xK_equal ), sendMessage Mag.MagnifyMore)
<    , ((modMask .|. controlMask              , xK_minus), sendMessage Mag.MagnifyLess)
---
>     ((modMask .|. controlMask,               xK_Right ), sendMessage $ ExpandTowards R)
>   , ((modMask .|. controlMask .|. shiftMask, xK_Right ), sendMessage $ ShrinkFrom R)
>   , ((modMask .|. controlMask,               xK_Left  ), sendMessage $ ExpandTowards L)
>   , ((modMask .|. controlMask .|. shiftMask, xK_Left  ), sendMessage $ ShrinkFrom L)
>   , ((modMask .|. controlMask,               xK_Down  ), sendMessage $ ExpandTowards D)
>   , ((modMask .|. controlMask .|. shiftMask, xK_Down  ), sendMessage $ ShrinkFrom D)
>   , ((modMask .|. controlMask,               xK_Up    ), sendMessage $ ExpandTowards U)
>   , ((modMask .|. controlMask .|. shiftMask, xK_Up    ), sendMessage $ ShrinkFrom U)
>   , ((modMask,                               xK_r     ), sendMessage BSP.Rotate)
>   , ((modMask,                               xK_s     ), sendMessage BSP.Swap)
>   -- , ((modMask,                               xK_n     ), sendMessage BSP.FocusParent)
>   -- , ((modMask .|. controlMask,               xK_n     ), sendMessage BSP.SelectNode)
>   -- , ((modMask .|. shiftMask,                 xK_n     ), sendMessage BSP.MoveNode)
789,808d440
<   ++
<   [
<       ((modMask  .|. controlMask              , xK_s    ), sendMessage  Arrange         )
<       , ((modMask .|. controlMask .|. shiftMask, xK_s    ), sendMessage  DeArrange       )
<       , ((modMask.|. controlMask              , xK_Left ), sendMessage (MoveLeft      valueInt))
<       , ((modMask.|. controlMask              , xK_Right), sendMessage (MoveRight     valueInt))
<       , ((modMask.|. controlMask              , xK_Down ), sendMessage (MoveDown      valueInt))
<       , ((modMask.|. controlMask              , xK_Up   ), sendMessage (MoveUp        valueInt))
<       , ((modMask.|. shiftMask, xK_Left ), sendMessage (IncreaseLeft  valueInt))
<       , ((modMask.|. shiftMask, xK_Right), sendMessage (IncreaseRight valueInt))
<       , ((modMask.|. shiftMask, xK_Down), sendMessage (IncreaseDown valueInt))
<       , ((modMask.|. shiftMask, xK_Up), sendMessage (IncreaseUp valueInt))
<       , ((modMask.|. controlMask .|. shiftMask, xK_Left ), sendMessage (DecreaseLeft  valueInt))
<       , ((modMask.|. controlMask .|. shiftMask, xK_Right), sendMessage (DecreaseRight valueInt))
<       , ((modMask.|. controlMask .|. shiftMask, xK_Down ), sendMessage (DecreaseDown  valueInt))
<       , ((modMask.|. controlMask .|. shiftMask, xK_Up   ), sendMessage (DecreaseUp   valueInt))
<   ] 
<   where 
<     valueInt :: Int 
<     valueInt = 50
816c448
< myFocusFollowsMouse = False
---
> myFocusFollowsMouse = True
820,824c452,456
<     ((modMask .|. controlMask, button1),
<       -- (\w -> focus w >> mouseResizeWindow w >> afterDrag (withFocused $ windows . W.sink)))
<       (\w -> focus w >> mouseResizeWindow w))
<     , ((modMask, button1),
<     (\w -> focus w >> mouseMoveWindow w ))
---
>     -- mod-button1, Set the window to floating mode and move by dragging
>     ((modMask, button1),
>      (\w -> focus w >> mouseMoveWindow w))
> 
>     -- mod-button2, Raise the window to the top of the stack
825a458,461
>        (\w -> focus w >> windows W.swapMaster))
> 
>     -- mod-button3, Set the window to floating mode and resize by dragging
>     , ((modMask, button3),
826a463,464
> 
>     -- you may also bind events to the mouse scroll wheel (button4 and button5)
850,852c488
<   spawn     "~/.xmonad/startup.sh"
<   setWindowSpacingEnabled True
<   spawnOnce     "picom -b"
---
>   spawn     "bash ~/.xmonad/startup.sh"
855,887d490
< ------------------------------------------------------------------------
< 
< myBackgroundColor = background
< 
< myContentColor = foreground
< 
< 
< myXPromptConfig :: XPConfig
< myXPromptConfig =
<   XPC
<     { promptBorderWidth = 3
<     , font = "xft:Sarasa Gothic J:size=12:antialias=true"
<     , alwaysHighlight = True
<     , height = 30
<     , historySize = 256
<     , bgColor = myBackgroundColor
<     , fgColor = myContentColor
<     , bgHLight = color4
<     , fgHLight = myBackgroundColor
<     , borderColor = color4
<     , position = Top
<     , autoComplete = Nothing
<     , showCompletionOnTab = False
<     , searchPredicate = fuzzyMatch
<     , defaultPrompter = id
<     , sorter = const id
<     , maxComplRows = Just 5
<     , promptKeymap = defaultXPKeymap
<     , completionKey = (0, xK_Tab)
<     , changeModeKey = xK_grave
<     , historyFilter = id
<     , defaultText = []
<     }
890,892d492
< 
< padding = replicate 10 ' '
< 
894,895c494
< 
< 
---
> --
897,901d495
<   spawn     "nitrogen --restore &"
<   spawn     "~/.xmonad/startup.sh"
<   spawn     "nitrogen --restore"
<   -- spawn "feh --bg-center ~/.xmonad/1920x1200.jpg"
<   --
903,904c497
< 
< 
---
>   -- xmproc <- spawnPipe "taffybar"
910c503
<                                 -- , (mod4Mask .|. shiftMask, windowSwap)
---
>                                 , (mod4Mask .|. shiftMask, windowSwap)
913a507
>          -- $ pagerHints -- uncomment to use taffybar
916,925c510,512
<                 ppCurrent = xmobarColor xmobarCurrentForeground xmobarCurrentBackground . wrap ("<box type=Full color=" ++ color4 ++ ">")  " </box>"
<                 -- Hidden workspaces (no windows)
<                 , ppHiddenNoWindows = xmobarColor color8 "" .wrap ("<box type=Full color=" ++ background ++ ">")  " </box>"
<                  -- Visible but not current workspace (Xinerama only)
<                 , ppVisible = xmobarColor color4 "" .wrap ("<box type=Full color=" ++ background ++ ">")  " </box>"
<                  -- Hidden workspaces in xmobar
<                 , ppHidden = xmobarColor color4  "" .wrap ("<box type=Full color=" ++ background ++ ">")  " </box>"
<                 , ppSep = ""
<                , ppLayout = xmobarColor background color2 .wrap ("<action=xdotool key super+alt+space><box type=Full color=" ++ color2 ++ "><fn=1> ") " </fn></box></action>"
<                 , ppTitle = xmobarColor color2 "" . wrap ("<fn=2> ")  " </fn>"
---
>                   ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "" . wrap "[" "]"
>                 , ppTitle = xmobarColor xmobarTitleColor "" . shorten 50
>                 , ppSep = "   "
927,928c514
<          }
<          -- >> updatePointer (0.75, 0.75) (0.75, 0.75)
---
>          } >> updatePointer (0.75, 0.75) (0.75, 0.75)
932c518,524
< 
---
> -- Combine it all together
> -- A structure containing your configuration settings, overriding
> -- fields in the default config. Any you don't override, will
> -- use the defaults defined in xmonad/XMonad/Config.hs
> --
> -- No need to modify this.
> --
946,947c538
<     -- handleEventHook = handleEventHook def <+> docksEventHook <+> fullscreenEventHook,
<     -- handleEventHook = serverModeEventHookCmd <+> serverModeEventHook <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn) <+> docksEventHook <+> fullscreenEventHook,
---
> 
950,951c541,542
<     -- handleEventHook    = handleEventHook def <+> fullscreenEventHook,
<     handleEventHook =  fullscreenEventHook,
---
>     -- handleEventHook    = E.fullscreenEventHook,
>     handleEventHook    = fullscreenEventHook,
