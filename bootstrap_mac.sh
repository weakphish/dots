#!/bin/bash

# Install Homebrew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Need bundle command
brew tap Homebrew/bundle
brew bundle Brewfile
