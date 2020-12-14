#!/bin/bash

# Wallpaper 

# Power manager
if [ -z "$(pgrep xfce4-power-manager)" ];  then
    xfce4-power-manager &
fi


if [ -z "$(pgrep stalonetray)" ];  then
    stalonetray -c ~/.xmonad/stalonetrayrrc
fi

# # picom
if [ -z "$(pgrep picom)" ];  then
    picom -b
fi

# # Network Applet
if [ -z "$(pgrep nm-applet)" ];  then
    nm-applet &
fi


if [ -z "$(pgrep ibus-daemon)" ]; then
    ibus-daemon -drx &
fi

if [ -z "$(pgrep dunst)" ]; then
    dunst &
fi

if [ -z "$(pgrep udiskie)" ]; then
    udiskie &
fi
