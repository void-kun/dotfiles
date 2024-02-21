#!/usr/bin/env bash

# Terminate already running bar instances
# If all your bars have ipc enabled, you can use 
polybar-msg cmd quit
# Otherwise you can use the nuclear option:
# killall -q polybar

# Launch statusbar_bot
echo "---" | tee -a /tmp/statusbar_bot.log 
polybar statusbar_bot 2>&1 | tee -a /tmp/statusbar_bot.log & disown

echo "Bars launched..."