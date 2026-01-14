# Dotfiles, managed with Ansible & GNU Stow

# What

At work, I use MacOS primarily, but the tools listed below I use both at work and at home.

At home, I use Arch Linux.

## Main Tools
- Code Editor: Neovim, with JetBrains tooling for large refactors/debugging/anything Neovim struggles with.
- Shell: Zsh. I used to use Fish, but suffered from coworker-script-incompatibility-syndrome.
- Terminal: Ghostty on macOS/Linux
- Git Porcelain: LazyGit

## Other Software
Other software that I use as a developer on a regular basis:

- [A few Rust utilities](https://gist.github.com/sts10/daadbc2f403bdffad1b6d33aff016c0a)
  - Bat
  - Delta
  - Lsd
  - Ripgrep
  - Zellij as a terminal multi-plexer / Tmux alternative
- Good ol' pen and paper for note-taking
  - Plain markdown for longer-term notes or scratch documents

## Arch Linux

Currently using Hyprland, a tiling window manager. 

Included in this repo is configuration for Hypr and various related tools, 
since using a Tiling WM often means configuring a lot of your own desktop features
that you generally take for granted.

I use [nwg-look](https://github.com/nwg-piotr/nwg-look) to customize GTK apps and [hyprqt6engine](https://wiki.hypr.land/Hypr-Ecosystem/hyprqt6engine/) to customize QT themes.

## Game Development
For game development projects, I tend to stick to Godot. I use VS Code to edit GDScript, just because the built-in editor
is pretty basic, and I couldn't get Neovim to work with Godot reasonably well.

For C#, I use JetBrains Rider.

# Structure
- `config/` houses my dotfiles, which are symlinked with Stow. 
- `tasks/` contains the various tasks for Ansible to run when bootstrapping a system
- `vars/` contains variable definitions for Ansible, such as packages to install

# Usage
Run the bootstrap script for the appropriate operating system, then run `ansible-playbook --ask-become-pass setup.yml`

The bootstrap script for Mac just installs Homebrew and Ansible, whereas the Arch one just installs Ansible and configures yay for the AUR.
