#!/bin/sh

# Wallpaper 
# System tray
# if [ -z "$(pgrep trayer)" ] ; then
#     trayer --edge top \
#            --align right \
#            --widthtype percent \
#            --height 16 \
#            --alpha 0 \
#            --transparent true \
#            --width 5 &
# fi

# Power manager
if [ -z "$(pgrep xfce4-power-manager)" ] ; then
    xfce4-power-manager &
fi


if [ -z "$(pgrep stalonetray)" ] ; then
    stalonetray -c ~/.xmonad/stalonetrayrrc
fi

# if [ -z "$(pgrep pasystray)" ] ; then
#    pasystray &
# fi

# Taffybar
# if [ -z "$(pgrep taffybar)" ] ; then
#     taffybar &
# fi

# Redshift
# if [ -z "$(pgrep redshift)" ] ; then
    # redshift &
#fi

# Autolock
# if [ -z "$(pgrep xautolock)" ] ; then
    # xautolock -time 1 -locker "if ! grep 'RUNNING' /proc/asound/card*/pcm*/sub*/status;then xscreensaver-command -lock; else echo 'Sound on'; fi"
# fi

# Wallpaper
# if [ -z "$(pgrep nitrogen)" ] ; then
   # nitrogen --restore &
# fi


# Screensaver
# if [ -z "$(pgrep xscreensaver)" ] ; then
#    xscreensaver -no-splash &
# fi

# picom
# if [ -z "$(pgrep picom)" ] ; then
#     picom -c -b
# fi

# Network Applet
if [ -z "$(pgrep nm-applet)" ] ; then
    nm-applet &
fi

# Google Drive
# if [ -z "$(pgrep insync)" ] ; then
#     insync start &
# fi

if [ -z "$(pgrep fcitx)" ] ; then
    fcitx -dr
fi

if [ -z "$(pgrep dunst)" ] ; then
    dunst &
fi

if [ -z "$(pgrep udiskie)" ] ; then
    udiskie &
fi

if [ -z "$(pgrep xscreensaver)" ] ; then
    xscreensaver &
fi
