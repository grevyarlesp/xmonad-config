-- xmonad config used by k4l1brx

import System.IO
import Data.Monoid
import Data.Ratio ((%))
import Data.Word
import Control.Monad ((>=>), join, liftM, when)   -- For Custom Fullscreen Function
import GHC.Word
import System.Exit
import qualified Data.List as L
import XMonad.Util.Font
import XMonad.Layout.AvoidFloats
import XMonad hiding ( (|||) )
import XMonad.Actions.Navigation2D
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Util.SpawnOnce 
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Layout.DecorationMadness
import XMonad.Layout.BorderResize

import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.Place
import XMonad.Hooks.InsertPosition

import XMonad.Layout hiding ( (|||) )
import XMonad.Layout.TabBarDecoration
import XMonad.Layout.Gaps
import XMonad.Layout.Fullscreen
import XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.NoBorders

-- Layouts 
import XMonad.Layout.Tabbed
import XMonad.Layout.SimplestFloat
import XMonad.Layout.SimpleFloat
import XMonad.Layout.WindowArranger
import XMonad.Layout.ThreeColumns
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.ResizableTile -- for resizeable tall layout 
import XMonad.Layout.MouseResizableTile -- for mouse control  
--- Layout Modifiers 
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Spacing
import XMonad.Layout.MultiToggle
import qualified XMonad.Layout.ToggleLayouts as TL
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ZoomRow
import XMonad.Layout.Grid
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Cursor

import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.FloatKeys
-- Tree Select 
import Data.Tree
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Hooks.WorkspaceHistory
import qualified XMonad.StackSet as W

import XMonad.Actions.FloatSnap
import XMonad.Actions.Promote

import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch

import XMonad.Util.NamedScratchpad
import XMonad.Prompt.Shell 

----------------------------mupdf--------------------------------------------
-- Terminimport XMonad.Hooks.EwmhDesktopsal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--

myTerminal = "~/.scripts/launch_kitty.sh"

-- The command to lock the screen or show the screensaver.
myScreensaver = "dm-tool switch-to-greeter"

-- The command to take a selective screenshot, where you select
-- what you'd like to capture on the screen.
mySelectScreenshot = "spectacle -r"

-- The command to take a fullscreen screenshot.
myScreenshot = "spectacle -f"

-- The command to use as a launcher, to launch commands that don't have
-- preset keybindings.
myLauncher = "~/.scripts/rofi_app_launcher.sh"


-----------------------------------------------------------------------
------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
-- myWorkspaces = ["1: term","2: web","3: "","4: media"] ++ map show [5..9]

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . map xmobarEscape
               $ ["1. main", "2. web", "3. docs", "4. code1", "5. code2", "6. code", "7. write", "8. edit", "9. watch"]
  where
        clickable l = [ "<action=xdotool key super+" ++ show n ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..9] l,
                      let n = i ]

    -- [((m .|. modMask, k), windows $ f i)
    --   | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    --   , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
------------------------

-- Android studio fix 
(~=?) :: Eq a => Query [a] -> [a] -> Query Bool
q ~=? x = fmap (L.isInfixOf x) q
-- Do not treat menus and settings popup as a separate window.
manageIdeaCompletionWindow = (className =? "jetbrains-studio") <&&> (title ~=? "win") --> doIgnore

myManageHook = 
      (isDialog --> doF W.swapUp)                       -- Bring Dialog Window on Top of Parent Floating Window
       <+> insertPosition Below Newer                    -- Insert New Windows at the Bottom of Stack Area
       <+> manageIdeaCompletionWindow                    -- Adding Fix for Android Studio
       <+>
      composeAll
      [
        className =? "firefox"                --> doShift "2. web"
      , resource  =? "desktop_window"               --> doIgnore
      , className =? "Galculator"                   --> doCenterFloat
      , className =? "Steam"                        --> doCenterFloat
      , className =? "Gimp"                         --> doCenterFloat
      , resource  =? "gpicview"                     --> doCenterFloat
      , className =? "MPlayer"                      --> doCenterFloat
      , className =? "Pavucontrol"                  --> doCenterFloat
      -- , className =? "Mate-power-preferences"       --> doCenterFloat
      , className =? "Xfce4-power-manager-settings" --> doCenterFloat
      -- , className =? "VirtualBox"                   --> doShift "4:vm"
      , className =? "Xchat"                        --> doShift "5:media"
      , className =? "stalonetray"                  --> doIgnore
      -- , isFullscreen                                --> (doF W.focusDown <+> doFullFloat)
      , isFullscreen                             --> doFullFloat
      ]



------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.

outerGaps    = 0
myGaps       = gaps [(U, outerGaps), (R, outerGaps), (L, outerGaps), (D, outerGaps)]
addSpace     = renamed [CutWordsLeft 2] . spacing gap
tab          = avoidStruts
               $ renamed [Replace "Tabbed"]
               --- $ addTopBar
               --- $ addTabsBottomAlways
                -- $ addTabsBottomAlways shrinkText myTabTheme tabbed
              $ myGaps
              $ addSpace
               $ tabbedBottomAlways shrinkText myTabTheme




myFloat = renamed [Replace "Floating"]
          -- $ noFrillsDeco shrinkText myTabTheme 
          $ addSpace
          $ myGaps
          $  tabBar shrinkText myTabTheme Bottom (simplestFloat)
          -- $ tabBar shrinkText myTabTheme Bottom (gaps[(D, 18)] $ (simpleFloat' shrinkText myTabTheme))
          -- $ floatDefault shrinkText myTabTheme

-- (renamed [CutWordsLeft 1]
--                   $ addTopBar
--                   $ windowNavigation
--                   $ renamed [Replace "BSP"]
--                   $ addTabs shrinkText myTabTheme
--                   $ subLayout [] Simplest
--                   $ myGaps
--                   $ addSpace (BSP.emptyBSP)
--                 )

layouts      = TL.toggleLayouts myFloat (borderResize (windowArrange (tab ||| avoidStruts (
                  (
                  -- addTopBar
                  renamed [Replace "3C"]
                  $ windowNavigation
                  $ addSpace
                  $ myGaps
                  $ tabBar shrinkText myTabTheme Bottom (gaps[(D, 18)] $ ThreeColMid 1 (3/100) (1/2))
                  -- $ renamed [CutWordsLeft 2]
                ) ||| (
                  --- $ addTopBar
                  windowNavigation
                  $ renamed [Replace "Grid"]
                  $ myGaps
                  $ addSpace
                  $  tabBar shrinkText myTabTheme Bottom (gaps [(D, 18)] $ Grid)
                  )))))


myLayout    = smartBorders
              $ mkToggle (NOBORDERS ?? FULL ?? EOT)
              $ layouts

myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [("Full",          centerNavigation)
    -- line/center same results   ,("Tabs", lineNavigation)
    --                            ,("Tabs", centerNavigation)
                                  ]
    , unmappedWindowRect        = [("Full", singleWindowRect)
    -- works but breaks tab deco  ,("Tabs", singleWindowRect)
    -- doesn't work but deco ok   ,("Tabs", fullScreenRect)
                                  ]
    }
-------
-- Tree Menu
treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction myMenu = TS.treeselectAction myMenu
  [
    Node (TS.TSNode "Session" "Session" (return())) 
     [
       Node(TS.TSNode "Lock" "Lock the system" (spawn "dm-tool switch-to-greeter")) []
       , Node(TS.TSNode "Suspend" "Suspend" (spawn "systemctl suspend")) []
       , Node(TS.TSNode "Logout" "logout" (io (exitWith ExitSuccess))) []
       , Node(TS.TSNode "Shutdown" "Poweroff the system" (spawn "systemctl poweroff")) []
     ]
   , Node (TS.TSNode "Brightness" "Sets screen brightness using xbacklight" (return ()))
       [ Node (TS.TSNode "100%" "FULL POWER!!"            (spawn "xbacklight -set 100")) []
       , Node (TS.TSNode "75%" "Normal Brightness (75%)" (spawn "xbacklight -set 75"))  []
       , Node (TS.TSNode "50%" "Normal Brightness (50%)" (spawn "xbacklight -set 50"))  []
       , Node (TS.TSNode "25%"    "Quite dark"              (spawn "xbacklight -set 25"))  []
       ]
   ]
tsDefaultConfig :: TS.TSConfig myMenu
tsDefaultConfig = TS.TSConfig { 
                             TS.ts_hidechildren = True
                           , TS.ts_background   = 0x00000000
                           , TS.ts_font         = "xft:Sans-16"
                           , TS.ts_node         = (0xff000000, 0xff50d0db)
                           , TS.ts_nodealt      = (0xff000000, 0xff10b8d6)
                           , TS.ts_highlight    = (0xffffffff, 0xffff0000)
                           , TS.ts_extra        = 0xff0000ff
                           , TS.ts_node_width   = 200
                           , TS.ts_node_height  = 30
                           , TS.ts_originX      = 0
                           , TS.ts_originY      = 0
                           , TS.ts_indent       = 80
                           , TS.ts_navigate     = myTreeNavigation
                           }
myTreeNavigation = M.fromList
    [ ((0, xK_Escape),   TS.cancel)
    , ((0, xK_Return),   TS.select)
    , ((0, xK_space),    TS.select)
    , ((0, xK_Up),       TS.movePrev)
    , ((0, xK_Down),     TS.moveNext)
    , ((0, xK_Left),     TS.moveParent)
    , ((0, xK_Right),    TS.moveChild)
    , ((0, xK_k),        TS.movePrev)
    , ((0, xK_j),        TS.moveNext)
    , ((0, xK_h),        TS.moveParent)
    , ((0, xK_l),        TS.moveChild)
    , ((0, xK_o),        TS.moveHistBack)
    , ((0, xK_i),        TS.moveHistForward)
    , ((0, xK_d),        TS.moveTo ["dev"])
    , ((0, xK_g),        TS.moveTo ["graphics"])
    , ((0, xK_m),        TS.moveTo ["music"])
    , ((0, xK_v),        TS.moveTo ["video"])
    , ((0, xK_w),        TS.moveTo ["web"])
    , ((mod4Mask, xK_b), TS.moveTo ["web", "browser"])
    , ((mod4Mask, xK_c), TS.moveTo ["web", "chat"])
    , ((mod4Mask, xK_m), TS.moveTo ["web", "email"])
    , ((mod4Mask, xK_r), TS.moveTo ["web", "rss"])
    , ((mod4Mask, xK_w), TS.moveTo ["web", "web conference"])
    , ((mod4Mask, xK_d), TS.moveTo ["dev", "docs"])
    , ((mod4Mask, xK_e), TS.moveTo ["dev", "emacs"])
    , ((mod4Mask, xK_f), TS.moveTo ["dev", "files"])
    , ((mod4Mask, xK_p), TS.moveTo ["dev", "programming"])
    , ((mod4Mask, xK_t), TS.moveTo ["dev", "terminal"])
    , ((mod4Mask, xK_z), TS.moveTo ["dev", "virtualization"])
    , ((mod4Mask, xK_g), TS.moveTo ["graphics", "gimp"])
    , ((mod4Mask, xK_i), TS.moveTo ["graphics", "image viewer"])
    , ((mod4Mask, xK_a), TS.moveTo ["music", "audio editor"])
    , ((mod4Mask, xK_u), TS.moveTo ["music", "music player"])
    , ((mod4Mask, xK_o), TS.moveTo ["video", "obs"])
    , ((mod4Mask, xK_v), TS.moveTo ["video", "video player"])
    , ((mod4Mask, xK_k), TS.moveTo ["video", "kdenlive"])
    , ((mod4Mask .|. altMask, xK_h), TS.moveTo ["dev", "programming", "haskell"])
    , ((mod4Mask .|. altMask, xK_p), TS.moveTo ["dev", "programming", "python"])
    , ((mod4Mask .|. altMask, xK_s), TS.moveTo ["dev", "programming", "shell"])
    ]
------------------------------------------------------------------------
-- Colors and borders

-- Color of current window title in xmobar.
xmobarTitleColor = "green"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#51AFEF"

-- Width of the window border in pixels.
myBorderWidth = 1

myNormalBorderColor     = "#000000"
myFocusedBorderColor    = active

base03  = "#002b36"
base02  = "#073642"
base01  = "#586e75"
base00  = "#657b83"
base0   = "#839496"
base1   = "#93a1a1"
base2   = "#eee8d5"
base3   = "#fdf6e3"
yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"

-- sizes
gap         = 2
topbar      = 5
border      = 0
prompt      = 5
status      = 5

active      = blue
activeWarn  = red
inactive    = base02
focusColor  = blue
unfocusColor = base02

-- myFont      = "-*-Zekton-medium-*-*-*-*-160-*-*-*-*-*-*"
-- myBigFont   = "-*-Zekton-medium-*-*-*-*-240-*-*-*-*-*-*"
-- myFont      = "xft:Inconsolata Nerd Font:size=9:bold:antialias=true"
-- myBigFont   = "xft:Inconsolata Nerd Font:size=9:bold:antialias=true"
-- myWideFont  = "xft:Inconsolata Nerd Font:"
--             ++ "style=Regular:pixelsize=180:hinting=true"

myFont      = "Utf8:JetBrains Mono:size=9:bold:antialias=true"
myBigFont   = "Utf8:JetBrains Mono:size=9:bold:antialias=true"
myWideFont  = "Utf8:JetBrains Mono:"
            ++ "style=Regular:pixelsize=180:hinting=true"
-- this is a "fake title" used as a highlight bar in lieu of full borders
-- (I find this a cleaner and less visually intrusive solution)
topBarTheme = def
    {
      fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

-- addTopBar =  noFrillsDeco shrinkText topBarTheme

myTabTheme = def
    { fontName = "xft:monospace:pixelsize=10:antialias=true"
    , activeColor           = base02
    , inactiveColor         = "#000000"
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = "#FFFFFF"
    , inactiveTextColor     = "#FFFFFF"
    , decoHeight            = 18
    }

--

scratchpads = [
  NS "todo" spawnTerm findTerm manageTerm,
  NS "wiki" spawnWiki findWiki nonFloating
              ]
  where 
    role = stringProperty "WM_WINDOW_ROLE"
    spawnTerm = "GLFW_IM_MODULE=ibus kitty --name scratchpad --session ~/.config/kitty/todo.conf"
    spawnWiki = "GLFW_IM_MODULE=ibus kitty --name scratchpad_wiki --session ~/.config/kitty/vimwiki.conf"
    findTerm = resource =? "scratchpad"
    findWiki = resource =? "scratchpad_wiki"
    manageTerm = customFloating $ W.RationalRect l t w h -- and I'd like it fixed using the geometry below
      where
        -- reusing these variables is ok since they're confined to their own 
        -- where clauses 
        h = (1/2)       -- height, 10% 
        w = (1/2)       -- width, 100%
        t = 0.1            -- bottom edge
        l = (1 - w)/2-- centered left/right


------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--

myModMask = mod4Mask
altMask = mod1Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask, xK_d),
     treeselectAction tsDefaultConfig)
   , ((modMask .|. altMask, xK_Return),
     spawn $ XMonad.terminal conf)

  -- Lock the screen using command specified by myScreensaver.
  , ((modMask, xK_0),
     spawn myScreensaver)

  -- Spawn the launcher using command specified by myLauncher.
  -- Use this to launch programs without a key binding.
  , ((modMask, xK_p),
     spawn myLauncher)

  -- Take a selective screenshot using the command specified by mySelectScreenshot.
  , ((modMask .|. shiftMask, xK_p),
     spawn mySelectScreenshot)

  , ((modMask .|. shiftMask, xK_Return),
      spawn "dolphin \"`xcwd`\"")
  -- Take a full screenshot using the command specified by myScreenshot.
  , ((modMask, xK_Print),
     spawn myScreenshot)

  , ((modMask .|. shiftMask, xK_Print),
     spawn mySelectScreenshot)

  , ((modMask, xK_space),
     spawn "~/.scripts/Toggle_Ibus.sh")

  , ((controlMask .|. altMask, xK_k),
     spawn "~/.scripts/Toggle_Keymap.sh")
  -- Toggle current focus window to fullscreen
  , ((modMask, xK_f), sendMessage $ Toggle FULL)
  , ((modMask, xK_s), sendMessage $ TL.Toggle "Floating")
  , ((modMask .|. altMask, xK_s), sendMessage $ JumpToLayout "Tabbed")

  -- Mute volume.
  , ((0, xF86XK_AudioMute),
     spawn "~/.scripts/volume_change.sh toggle")

  -- Decrease volume.
  , ((0, xF86XK_AudioLowerVolume),
     spawn "~/.scripts/volume_change.sh 4%-")

  -- Increase volume.
  , ((0, xF86XK_AudioRaiseVolume),
     spawn "~/.scripts/volume_change.sh 4%+")

  , ((0, xF86XK_MonBrightnessUp),
      spawn "~/.scripts/brightness_change.sh +")
          
  , ((0, xF86XK_MonBrightnessDown),
      spawn "~/.scripts/brightness_change.sh -")
  -- Audio previous.
  , ((0, 0x1008FF16),
     spawn "")

  -- Play/pause.
  , ((0, 0x1008FF14),
     spawn "")

  -- Audio next.
  , ((0, 0x1008FF17),
     spawn "")

  -- Eject CD tray.
  , ((0, 0x1008FF2C),
     spawn "eject -T")

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Close focused window.
  , ((modMask .|. shiftMask, xK_w),
     kill)

  -- Cycle through the available layout algorithms.
  , ((modMask .|. altMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp)

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

  -- Quit xmonad.
  , ((modMask .|. altMask, xK_q),
     io (exitWith ExitSuccess))

  -- Restart xmonad.
  , ((modMask .|. altMask, xK_r),
     restart "xmonad" True)
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  -- [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
  --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
  --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  -- ++
  -- Bindings for manage sub tabs in layouts please checkout the link below for reference
  -- https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Layout-SubLayouts.html
  [
    -- Tab current focused window with the window to the left
    ((modMask .|. controlMask, xK_h), sendMessage $ pullGroup L)
    -- Tab current focused window with the window to the right
  , ((modMask .|. controlMask, xK_l), sendMessage $ pullGroup R)
    -- Tab current focused window with the window above
  , ((modMask .|. controlMask, xK_k), sendMessage $ pullGroup U)
    -- Tab current focused window with the window below
  , ((modMask .|. controlMask, xK_j), sendMessage $ pullGroup D)

  -- Tab all windows in the current workspace with current window as the focus
  , ((modMask .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
  -- Group the current tabbed windows
  , ((modMask .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))

  -- Toggle through tabes from the right
  , ((modMask, xK_Tab), onGroup W.focusDown')
  ]

  ++
  [ ((modMask, xK_u), namedScratchpadAction scratchpads "todo")
   , ((modMask, xK_i), namedScratchpadAction scratchpads "wiki")
  ]
  ++
  [
    ((modMask, xK_o), shellPrompt myXPromptConfig)
  ]
  ++
  [
      ((modMask  .|. controlMask              , xK_s    ), sendMessage  Arrange         )
      , ((modMask .|. controlMask .|. shiftMask, xK_s    ), sendMessage  DeArrange       )
      , ((modMask.|. controlMask              , xK_Left ), sendMessage (MoveLeft      valueInt))
      , ((modMask.|. controlMask              , xK_Right), sendMessage (MoveRight     valueInt))
      , ((modMask.|. controlMask              , xK_Down ), sendMessage (MoveDown      valueInt))
      , ((modMask.|. controlMask              , xK_Up   ), sendMessage (MoveUp        valueInt))
      , ((modMask.|. shiftMask, xK_Left ), sendMessage (IncreaseLeft  valueInt))
      , ((modMask.|. shiftMask, xK_Right), sendMessage (IncreaseRight valueInt))
      , ((modMask.|. shiftMask, xK_Down), sendMessage (IncreaseDown valueInt))
      , ((modMask.|. shiftMask, xK_Up), sendMessage (IncreaseUp valueInt))
      , ((modMask.|. controlMask .|. shiftMask, xK_Left ), sendMessage (DecreaseLeft  valueInt))
      , ((modMask.|. controlMask .|. shiftMask, xK_Right), sendMessage (DecreaseRight valueInt))
      , ((modMask.|. controlMask .|. shiftMask, xK_Down ), sendMessage (DecreaseDown  valueInt))
      , ((modMask.|. controlMask .|. shiftMask, xK_Up   ), sendMessage (DecreaseUp   valueInt))
  ] 
  where 
    valueInt :: Int 
    valueInt = 50

------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    ((modMask .|. controlMask, button1),
      -- (\w -> focus w >> mouseResizeWindow w >> afterDrag (withFocused $ windows . W.sink)))
      (\w -> focus w >> mouseResizeWindow w))
    , ((modMask, button1),
    (\w -> focus w >> mouseMoveWindow w ))
    , ((modMask, button2),
       (\w -> focus w >> mouseResizeWindow w))
  ]


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--


------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  setWMName "Xmonad"
  spawn     "~/.xmonad/startup.sh"

  spawn     "picom -b"
  setDefaultCursor xC_left_ptr

------------------------------------------------------------------------

myBackgroundColor = "#151515"

myContentColor = "#d0d0d0"


myXPromptConfig :: XPConfig
myXPromptConfig =
  XPC
    { promptBorderWidth = 1
    , alwaysHighlight = True
    , height = 22
    , historySize = 256
    , font = myFont
    , bgColor = myBackgroundColor
    , fgColor = myContentColor
    , bgHLight = myBackgroundColor
    , fgHLight = myContentColor
    , borderColor = myBackgroundColor
    , position = Top
    , autoComplete = Nothing
    , showCompletionOnTab = False
    , searchPredicate = fuzzyMatch
    , defaultPrompter = id
    , sorter = const id
    , maxComplRows = Just 7
    , promptKeymap = defaultXPKeymap
    , completionKey = (0, xK_Tab)
    , changeModeKey = xK_grave
    , historyFilter = id
    , defaultText = []
    }

------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--

main = do

  spawn     "nitrogen --restore &"
  spawn     "~/.xmonad/startup.sh"
  spawn     "nitrogen --restore"
  -- spawn "feh --bg-center ~/.xmonad/1920x1200.jpg"
  --
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobarrc.hs"
  -- spawn "killall xmobar"
  -- restart "xmonad" True
  xmonad $ docks
         $ withNavigation2DConfig myNav2DConf
         $ additionalNav2DKeys (xK_Up, xK_Left, xK_Down, xK_Right)
                               [
                                  (mod4Mask,               windowGo  )
                                -- , (mod4Mask .|. shiftMask, windowSwap)
                               ]
                               False
         -- $ ewmh
         $ ewmh
         -- $ pagerHints -- uncomment to use taffybar
         $ defaults {
         logHook = dynamicLogWithPP xmobarPP {
                -- ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "" . wrap "[" "]"
                ppCurrent = xmobarColor "black" "#008894" . wrap "" ""
                , ppVisible = xmobarColor "#c3e88d" "green"                -- Visible but not current workspace
                -- , ppHidden = xmobarColor "#82AAFF" "" . wrap "*" ""   -- Hidden workspaces in xmobar
                , ppHiddenNoWindows = xmobarColor "#c792ea" ""        -- Hidden workspaces (no windows)
                , ppTitle = xmobarColor xmobarTitleColor "" . shorten 45
                -- , ppTitle = "#FFD700" xmobarTitleColor "" . shorten 50
                , ppSep = "|"
               , ppLayout = xmobarColor "#FFD700" ""
                , ppOutput = hPutStrLn xmproc
         } >> updatePointer (0.75, 0.75) (0.75, 0.75)
      }

------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--

defaults = def {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = myLayout,
    -- handleEventHook    = E.fullscreenEventHook,
    handleEventHook    = handleEventHook def <+> fullscreenEventHook,
    manageHook         =   namedScratchpadManageHook scratchpads <+> manageDocks <+> myManageHook,
    -- manageHook         =   manageDocks <+> myManageHook <+> placeHook simpleSmart,
    startupHook        = myStartupHook
}
