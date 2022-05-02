Config { 

   -- appearance
     font =         "xft:Sarasa Fixed SC Nerd Font:size=15:regular:antialias=true"
   , bgColor =      "#3b4252"
   , fgColor =      "#eceff4"
   , position =     Top
   , border =       BottomBM 1
   , borderColor =  "#8fbcbb"

   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = "   %StdinReader% } { %date% "

   -- general behavior
   , lowerOnStart =     True    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest =     False   -- choose widest display (multi-monitor)
   , persistent =       True    -- enable/disable hiding (True = disabled)
   , commands = [
   	Run Weather "ZSFZ" [
		"--template", "<skyCondition> <fc=#88c0d0><tempC></fc>°C"
		] 36000
	, Run Cpu [
		"-L", "10", "-H", "80"
		, "--normal", "#d08770", "--high", "#bf616a", "--low", "#a3be8c"
		] 10
	, Run Memory [
		"--template", "Mem: <usedratio>%"
		, "--Low", "20" -- units: %
		, "--High", "90" -- units: %
		, "--low", "#a3be8c"
		, "--normal", "#d08770"
		, "--high" , "#bf616a"
		] 10
	, Run Date "<fc=#e5e9f0>%F (%a) %T</fc>" "date" 10
	, Run StdinReader
        ]
   }

