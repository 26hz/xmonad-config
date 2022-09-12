#!/usr/bin/env bash

# Terminate already running bar instances
pkill polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch polybar
polybar top -c $HOME/.config/xmonad/polybar/config-top.ini &
polybar bottom -c $HOME/.config/xmonad/polybar/config-bottom.ini &
