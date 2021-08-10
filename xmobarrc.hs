-- http://projects.haskell.org/xmobar/
-- install xmobar with these flags: --flags="with_alsa" --flags="with_mpd" --flags="with_xft"  OR --flags="all_extensions"
-- you can find weather location codes here: http://weather.noaa.gov/index.html


Config { font    = "xft:Sarasa Gothic J:pixelsize=12:antialias=true:hinting=true:style=Semibold,Inconsolata Nerd Font:pixelsize=12:antialias=true:hinting=true"
       , additionalFonts = [ "xft:Hack Nerd Font:pixelsize=12:hinting=true:antialias=true",
            "xft:Sarasa Gothic J:pixelsize=12:style=Semibold"
       ]
       , bgColor = "#1d2021"
       , alpha = 255
       , fgColor = "#00acc1"
       -- On the Top, 1000% screen width
       , position = Top
       , lowerOnStart = True , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , iconRoot = "/home/hts/.xmonad/xpm/"  -- default: "."
       , commands = [
                    Run Com "/home/hts/.xmonad/trayer-padding-icon.sh" [] "trayerpad" 10
                    , Run Battery       [ "--template" , "<box type=Full color=#b8bb26> <leftipat> <acstatus> </box>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "#fa4934"
                             , "--normal"   , "#fabd2f"
                             , "--high"     , "#b8bb26"
                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o", "<left>% (<timeleft>)"
                                       -- AC "on" status
                                       , "-O", " <left>%"
                                       -- chaged status
                                       , "-i", "<fc=#b8bb26>Full</fc>"
                                        , "--on-icon-pattern", "<fc=#daa520>\xf583</fc>  "
                                        , "--idle-icon-pattern", "<fc=#b8bb26>\xf584  </fc>"
                                        , "--highs", "<fc=#b8bb26>\xf581  </fc>"
                                        , "--mediums", "<fc=#fabd2f>\xf57d  </fc> "
                                        , "--lows", "<fc=#fa4934>\xf57a  </fc> "
                             ] 50

                      -- Time and date
                    , Run Date "<box type=Full color=#8ec07c><fc=#8ec07c> \xf5ef  <fc=#fabd2f>%I:%M %p</fc> %a %d %m %Y </fc></box>" "date" 10
                    , Run DynNetwork     [ "--template" ,"<box type=Full color=#8ec07c> <fc=#8ec07c>\xf0e8  \xf175<rx>KB \xf176<tx>KB </fc></box>"
                              ,"--Low"      , "1000"       -- units: B/s
                             , "--High"     , "5000"       -- units: B/s
                             , "--low"      , "#fa4934"
                             , "--normal"   , "#fabd2f"
                             , "--high"     , "#b8bb26"
                             , "--" 
                                  , "--devices", "p4p2"
                             ] 10
                    , Run Wireless "" [
                    "--template", " <box type=Full color=#8ec07c> <fc=#8ec07c>\xfaa8  <qualityipat><qualitybar></fc> </box>"
                    -- ,"--", "--quality-icon-pattern", "123"
                    
                    ] 50

                      -- Volume control
                    , Run Alsa "pulse" "Master" ["-t", "<box type=Full color=#B8BB26> <fc=#B8BB26>\xf028  <volume>%<status></fc> </box>" 
                    -- , "--" , "--on", "<fc=#B8BB26>[on]"
                    ]
                    -- , Run Kbd            [ ("de" , "<box type=Full color=#fabd2f> <fc=#FABD2F>\xf40b  DE</fc> </box>")
                    --          , ("us"         , "<box type=Full color=#fabd2f> <fc=#FABD2F>\xf40b  US</fc> </box>")
                    --          ]
                      -- Prints out the left side items such as workspaces, layout, etc.
                      -- The workspaces are set to be 'clickable' in .xmonad/xmonad.hs
                    -- , Run UnsafeStdinReader
                    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "<action=`~/.scripts/rofi_app_launcher.sh`><fc=#1d2021,#83a598><box type=Full color=#83a598>    </box></fc></action>%UnsafeStdinReader% }{<action=`kitty --session ~/.config/kitty/nmtui.conf`>%dynnetwork%%wi%</action><action=`xfce4-power-manager-settings`> %battery% </action>%alsa:pulse:Master%<box type=Full color=#fa4934></box> %date% <box type=Full color=#83a598>%trayerpad%</box>"
       }
