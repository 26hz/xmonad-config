-- Imports
import XMonad hiding ( (|||) )
import XMonad.Operations
import System.IO
import System.Exit
import Data.Monoid
import Data.Maybe
import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.SpawnNamedPipe
import XMonad.Util.NamedScratchpad
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

import XMonad.Prompt
import XMonad.Prompt.Shell

import XMonad.Actions.SpawnOn
import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D
import XMonad.Actions.RotSlaves
import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WithAll (killAll)
import XMonad.Actions.FloatKeys (keysResizeWindow, keysAbsResizeWindow)

import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive

import XMonad.Layout.Renamed
--import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.NoBorders
import XMonad.Layout.CenteredMaster
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.SubLayouts
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Circle
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Simplest
import XMonad.Layout.LayoutHints
import XMonad.Layout.TwoPane
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spiral
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.Fullscreen
import XMonad.Layout.ToggleLayouts          -- Full window at any time
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Mosaic
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Groups.Examples

----Main Function
main :: IO ()
main = do
    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    xmonad $ ewmh $ docks $ withUrgencyHook NoUrgencyHook $ defaults { logHook = dynamicLogWithPP (polybarHook dbus) }

-- Override the PP values as you would otherwise, adding colors etc depending
-- on  the statusbar used
polybarHook :: D.Client -> PP
polybarHook dbus = def
    { ppOutput = dbusOutput dbus }
-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

defaults = def{
    modMask = mod4Mask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , keys = myKeys
    --, layoutHook = smartBorders $ myLayoutHook
    , layoutHook = myLayout
    , focusedBorderColor = "#88c0d0"
    , normalBorderColor = "#2e3440"
    , mouseBindings = myMouseBindings                           
    , manageHook = myManageHook <+> manageHook def
    , handleEventHook = myEventHook
    , borderWidth         = 3
    , startupHook = myStartupHook
    }

myTerminal = "kitty"
ctrlMask = controlMask
altMask = mod1Mask
myBrowser = "qutebrowser -C $HOME/.xmonad/qutebrowser/config.py"
--myBrowser = "vivaldi"
myFileBrowser = "thunar"
myrofi = "rofi -show drun -theme $HOME/.xmonad/rofi/nord.rasi"
gsconfig1 = defaultGSConfig { gs_cellheight = 100, gs_cellwidth = 200 }

---- Key binding to toggle the gap for the bar.
--toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
togglePolybar = spawn "polybar-msg cmd toggle &"
toggleStruts  = togglePolybar >> sendMessage ToggleStruts
toggleFull    = togglePolybar >> sendMessage ToggleLayout
switchWS dir =
    findWorkspace filterOutNSP dir AnyWS 1 >>= windows . W.view
filterOutNSP = 
    let g f xs = filter (\(W.Workspace t _ _) -> t /= "NSP") (f xs)
    in  g <$> getSortByIndex

myWorkspaces    = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 
    -- launch a terminal
    [ ((modm,               xK_Return), spawn myTerminal)
 
    -- launch rofi
    , ((modm,               xK_space ), spawn myrofi)
 
    , ((modm,               xK_v     ), spawn myBrowser)
    , ((modm,               xK_e     ), spawn myFileBrowser)
   -- close focused window    
    , ((modm,               xK_q     ), kill)
    , ((modm .|. shiftMask, xK_q     ), killAll)

    --, ((modm,               xK_d     ), goToSelected defaultGSConfig)
    --, ((modm .|. shiftMask, xK_d     ), spawnSelected defaultGSConfig ["kitty","telegram-desktop"])
    , ((modm,               xK_d     ), goToSelected $ gsconfig1)
    , ((modm .|. shiftMask, xK_d     ), spawnSelected defaultGSConfig ["kitty","telegram-desktop"])

    --- Rotate through the available layout algorithms
    , ((modm,               xK_p ), sendMessage NextLayout)
    , ((modm,               xK_r ), setLayout $ XMonad.layoutHook conf)
    , ((modm .|. shiftMask, xK_r ), withFocused (windows . W.sink))
    , ((modm,               xK_f ), toggleFull)
    , ((modm,               xK_g ), sendMessage $ JumpToLayout "Super Grid")
    , ((modm,               xK_t ), sendMessage $ JumpToLayout "Super Tall")
    , ((modm,               xK_s ), sendMessage $ JumpToLayout "Super Bsp")
    , ((modm,               xK_n ), sendMessage $ JumpToLayout "Super Two")
    , ((modm,               xK_c ), sendMessage $ JumpToLayout "Super Cir")

    , ((modm,               xK_z ), withFocused (keysResizeWindow (-10,-10) (1,1)))
    , ((modm,               xK_x ), withFocused (keysResizeWindow (10,10) (1,1)))
    , ((modm .|. shiftMask, xK_z ), withFocused (keysAbsResizeWindow (-10,-10) (1024,752)))
    , ((modm .|. shiftMask, xK_x ), withFocused (keysAbsResizeWindow (10,10) (1024,752)))

    , ((modm,               xK_o ), switchWS Next)
    , ((modm,               xK_i ), switchWS Prev)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next/previous window
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    -- Swap the focused window with the next/previous window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- Shrink/Expand the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Window navigation (ignores group's inner windows)
--    , ((modm,                 xK_Right), sendMessage $ Go R)
--    , ((modm,                 xK_Left ), sendMessage $ Go L)
--    , ((modm,                 xK_Up   ), sendMessage $ Go U)
--    , ((modm,                 xK_Down ), sendMessage $ Go D)
--    , ((modm .|. controlMask, xK_Right), sendMessage $ Swap R)
--    , ((modm .|. controlMask, xK_Left ), sendMessage $ Swap L)
--    , ((modm .|. controlMask, xK_Up   ), sendMessage $ Swap U)
--    , ((modm .|. controlMask, xK_Down ), sendMessage $ Swap D)
--    , ((modm,                             xK_h     ), windowGo L False)
--    , ((modm,                             xK_j     ), windowGo D False)
--    , ((modm,                             xK_k     ), windowGo U False)
--    , ((modm,                             xK_l     ), windowGo R False)
--    , ((modm .|. shiftMask,               xK_h     ), windowSwap L False)
--    , ((modm .|. shiftMask,               xK_j     ), windowSwap D False)
--    , ((modm .|. shiftMask,               xK_k     ), windowSwap U False)
--    , ((modm .|. shiftMask,               xK_l     ), windowSwap R False)
--    , ((modm .|. altMask,                 xK_h     ), sendMessage $ ExpandTowards L)
--    , ((modm .|. altMask,                 xK_j     ), sendMessage $ ExpandTowards D)
--    , ((modm .|. altMask,                 xK_k     ), sendMessage $ ExpandTowards U)
--    , ((modm .|. altMask,                 xK_l     ), sendMessage $ ExpandTowards R)
 
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
 
    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)
 
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
 
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
 
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    , ((modm               , xK_b     ), toggleStruts)
 
    -- Quit xmonad
    , ((modm .|. altMask   , xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm .|. altMask   , xK_r     ), spawn "xmonad --recompile; xmonad --restart")

    -- volume control
    , ((modm              , xK_Up    ), spawn "$HOME/.local/bin/media.sh up")
    , ((modm              , xK_Down  ), spawn "$HOME/.local/bin/media.sh down")
    , ((modm              , xK_Left  ), spawn "$HOME/.local/bin/web.sh down")
    , ((modm              , xK_Right ), spawn "$HOME/.local/bin/web.sh up")

    -- screenshot
    , ((ctrlMask .|. altMask , xK_a    ), spawn "$HOME/.local/bin/flames && notify-send -u normal 'Screenshot' 'successful'")
    , ((0                    , xK_Print), spawn "$HOME/.local/bin/maims && notify-send -u normal 'Screenshot' 'successful'")

    --, ((modm .|. ctrlMask, xK_g), sendMessage $ ToggleGaps)  -- toggle all gaps
    ]
     ++
    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++
 
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    --[((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
---spawn
--
myStartupHook = do
	spawnOnce "qjackctl"
	spawnOnce "pavucontrol"
	spawnOnce "ardour6 default"
	spawn "feh --bg-fill --no-fehbg ~/Pictures/wallpapers/nord_buildings.png"
	spawn "picom --experimental-backends --backend glx --xrender-sync-fence --config ~/.xmonad/picom/picom.conf"
	spawn "$HOME/.xmonad/polybar/launch.sh"
	spawn "fcitx5"
	spawn "parcellite"
	--setWMName "LG3D"

myManageHook = composeAll
      [ className =? "Thunar"         --> doFloat
      , className =? "Leafpad"        --> doFloat
      , className =? "Gimp"           --> doFloat
      , className =? "QjackCtl"       --> doShift "6"
      , className =? "Pavucontrol"    --> doShift "7"
      , className =? "Ardour"         --> doShift "8"
      , manageDocks
      , isFullscreen                --> (doF W.focusDown <+> doFullFloat)
      ]

myEventHook = hintsEventHook

-- Mouse bindings
 
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]


--myLayoutHook = avoidStruts (
       --toggleLayouts Full (Grid) ||| toggleLayouts Full (ThreeColMid 1 (1/20) (1/2)) ||| simpleTabbed ||| toggleLayouts Full (tiled) ||| Mirror tiled)
       --mybsp ||| myspiral ||| mygrid ||| simpleTabbed)
--        where
		-- default tiling algorithm partitions the screen into two panes
		--myspiral= mygaps $ spacing 4 $ spiral (6/7)

		--grid  = mygaps $ spacing 4 $ Grid
		--tiled   = mygaps $ spacing 4 $ Tall nmaster delta ratio
		--mybsp   = mygaps $ spacing 4 $ emptyBSP

		-- Gaps
		--mygaps = gaps [(U,8), (D,8), (L,8), (R,8)]

		-- The default number of windows in the master pane
		--nmaster = 1

		-- Percent of screen to increment by when resizing panes
		--delta = 2/100 

		-- Default proportion of screen occupied by master pane
		--ratio   = 1/2

gaps i = spacingRaw True (Border i i i i) True (Border i i i i) True 

myLayout  = toggleLayouts full 
            (   
            smartBorders 
                tiled2 
            ||| bsp
            ||| grid
            ||| twopanes 
            ||| circle
            ) 

full      = noBorders Full 

tiled2      = renamed [Replace "Super Tall"] 
            $ lessBorders Screen
            $ avoidStruts 
            $ windowNavigation 
            $ subLayout [] (Simplest ||| Circle)
            $ gaps 8 $ Tall 1 (3/100) (1/2) 

bsp       = renamed [Replace "Super Bsp"]
            $ lessBorders Screen
            $ avoidStruts
            $ windowNavigation 
	    $ gaps 8
	    $ emptyBSP

grid      = renamed [Replace "Super Grid"]
            $ lessBorders Screen
            $ avoidStruts
            $ windowNavigation
            $ gaps 8
              Grid 

twopanes  = renamed [Replace "Super Two"]
            $ lessBorders Screen
            $ avoidStruts
            $ windowNavigation
            $ gaps 8
            $ TwoPane (3/100) (1/2)

circle      = renamed [Replace "Super Cir"]
            $ windowNavigation
            $ lessBorders Screen
            $ avoidStruts Circle
		
