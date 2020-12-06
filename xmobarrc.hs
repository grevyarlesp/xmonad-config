-- http://projects.haskell.org/xmobar/
-- install xmobar with these flags: --flags="with_alsa" --flags="with_mpd" --flags="with_xft"  OR --flags="all_extensions"
-- you can find weather location codes here: http://weather.noaa.gov/index.html


Config { font    = "xft:Inconsolata Nerd Font:size=9:antialias=true:hinting=true"
       , additionalFonts = [ "xft:Wuncon Siji:pixelsize=13" ]
       , bgColor = "#black"
       , fgColor = "#00acc1"
       -- On the Top, 1000% screen width
       , position = Top
       , lowerOnStart = True , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , iconRoot = "/home/hts/.xmonad/xpm/"  -- default: "."
       , commands = [
                    Run Com "/home/hts/.xmonad/trayer-padding-icon.sh" [] "trayerpad" 10
                    , Run Battery        [ "--template" , "<fc=#83A598>\xf240  </fc><acstatus>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "darkred"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkgreen"

                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o", "<left>% (<timeleft>)"
                                       -- AC "on" status
                                       , "-O", "<fc=#dAA520>Charging</fc> <left>%"
                                       -- chaged status
                                       , "-i", "<fc=#b8bb26>Charged</fc>"
                             ] 50

                      -- Time and date
                    , Run Date "<fc=#FB4934>\xf133 %b %d %Y (%H:%M)</fc>" "date" 50
                    , Run DynNetwork     [ "--template" , "<fc=#8ec07c><dev> \xf175<rx>KB \xf176<tx>KB</fc>"
                              ,"--Low"      , "1000"       -- units: B/s
                             , "--High"     , "5000"       -- units: B/s
                             , "--low"      , "darkgreen"
                             , "--normal"   , "darkorange"
                             , "--high"     , "darkred"
                             ] 10
                    , Run Wireless "" [
                    "--template", "<fc=#8ec07c><qualitybar></fc>"
                    ] 50

                      -- Volume control
                    , Run Alsa "pulse" "Master" ["-t", "<fc=#B8BB26>\xf028 <volume>%<status></fc>" 
                      
                    ]

                    , Run Kbd            [ ("de" , "<fc=#FABD2F>\xf40b  DE</fc>")
                             , ("us"         , "<fc=#FABD2F>\xf40b  US</fc>")
                             ]

                      -- Prints out the left side items such as workspaces, layout, etc.
                      -- The workspaces are set to be 'clickable' in .xmonad/xmonad.hs
                    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
           , template = "<action=`~/.scripts/rofi_app_launcher.sh`><fc=#FABD2F><icon=haskell_20.xpm/></fc></action> %UnsafeStdinReader%}{<action=`~/.scripts/rofi_wifi_menu.sh`>%dynnetwork% %wi%</action><fc=#665c24>|</fc><action=`xfce4-power-manager-settings`> %battery% </action><fc=#665c24>|</fc><action=`~/.scripts/Toggle_Keymap.sh`>%kbd%</action><fc=#665c24>|</fc> %alsa:pulse:Master%<fc=#665c24>|</fc>%date% %trayerpad%"
       }
