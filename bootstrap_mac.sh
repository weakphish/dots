#!/bin/bash

# Install Homebrew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Need bundle command
brew tap Homebrew/bundle

# Install all packages in Brewfile
brew bundle

# Link dotfiles w/ stow
stow config --dotfiles

