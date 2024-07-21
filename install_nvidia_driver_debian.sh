#!/usr/bin/bash

sudo apt update -y
sudo apt upgrade -y

sudo apt -y install nvidia-detect

nvidia-detect

sudo apt install nvidia-driver
