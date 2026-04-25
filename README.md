# Dotfiles, managed with GNU Stow

# What

At work, I use MacOS primarily, but the tools listed below I use both at work and at home.

At home, I use MacOS on my personal laptop, and keep a Windows gaming computer despite my intense dislike for Windows.

## Main Tools
- Code Editor: Neovim and Zed, depending on the task.
- Shell: Fish
- Terminal: Ghostty
- Zellij as a tmux replacement/session-manager/terminal 10x improvement
- Git Porcelain: LazyGit

## Other Software
Other software that I use as a developer on a regular basis:

- [A few Rust utilities](https://gist.github.com/sts10/daadbc2f403bdffad1b6d33aff016c0a)
  - Bat
  - Delta
  - Lsd
  - Ripgrep
- Good ol' pen and paper for note-taking

## Game Development
For game development projects, I tend to stick to Godot. I use VS Code to edit GDScript, just because the built-in editor
is pretty basic, and I couldn't get Neovim to work with Godot reasonably well.

For C#, I use JetBrains Rider.

# Structure
- `config/` houses my dotfiles, which are symlinked with Stow
- `karabiner/` contains Karabiner-Elements configuration for macOS key remapping

# Usage
Run the bootstrap script for the appropriate operating system, then run `stow config --target ~/ --dotfiles`

The bootstrap script for Mac just installs Homebrew and Brewfile, whereas the Arch one just installs Ansible and configures yay for the AUR.
