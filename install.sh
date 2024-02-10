#!/usr/bin/bash

# emacs
echo "Install emacs, backup the exist .emacs.d folder"
EMACS_FOLDER = "~/.emacs.d/"

if [[ -d "$EMACS_FOLDER" ]]
   mv "$EMACS_FOLDER" "$EMACS_FOLDER"and"_bk"
   mkdir "$EMACS_FOLDER"
   ln -s "$EMACS_FOLDER"and"/"
then

fi
