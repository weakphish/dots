#!/bin/bash
# Update packages
sudo pacman -Syyu

# Get yay
sudo pacman -S --needed git base-devel && git clone https://aur.archlinux.org/yay-bin.git && cd yay-bin && makepkg -si

# Install ansible
sudo pacman -S ansible
