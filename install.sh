#!/usr/bin/bash

# emacs
echo "Install emacs, backup the exist .emacs.d folder"
EMACS_FOLDER = "~/.emacs.d/"

if [[ -d "$EMACS_FOLDER" ]]
then
    mv "$EMACS_FOLDER" "$EMACS_FOLDER"and"_bk"
    mkdir "$EMACS_FOLDER"
fi

ln -s "./.emacs.d/" "."

# i3 & i3status
echo "Update i3, i3status, alacritty, rofi config"
CONFIG_FOLDER = "~/.config/"

ln -s "./.config/i3" "$CONFIG_FOLDER"
ln -s "./.config/i3status" "$CONFIG_FOLDER"
cp -rf "./.config/alacritty/" "$CONFIG_FOLDER"
cp -rf "./.config/rofi/" "$CONFIG_FOLDER"
