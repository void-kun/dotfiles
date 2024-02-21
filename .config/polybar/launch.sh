#!/usr/bin/env bash

polybar-msg cmd quit

# Launch statusbar_bot
echo "---" | tee -a /tmp/statusbar_bot.log 
polybar statusbar_bot -c ~/.config/polybar/config/statusbar.ini 2>&1 | tee -a /tmp/statusbar_bot.log & disown

echo "Bars launched..."
