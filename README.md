# Dotfiles, managed with Ansible & GNU Stow

# What

At work, I use MacOS primarily, but the tools listed below I use both at work and at home.

At home, I use Arch Linux.

**Code Editor**: Neovim, with JetBrains tooling for large refactors/debugging/anything Neovim struggles with.

I've also been experimenting with both a vanilla config of Emacs, as well as Doom Emacs for both programming and Org Mode at work.

**Shell:** Fish

**Terminal:** Ghostty on macOS/Linux

**Git Porcelain:** LazyGit

**Other Software:**
Other software that I use as a developer on a regular basis:

- [A few Rust utilities](https://gist.github.com/sts10/daadbc2f403bdffad1b6d33aff016c0a)
  - Bat
  - Delta
  - Lsd
  - Ripgrep
- Good ol' pen and paper for note-taking
  - Plain markdown for longer-term notes or scratch documents

## Arch Linux

Currently, I'm using COSMIC from System76.

I used to use Hyprland as a tiling window manager. Included in this repo is configuration for Hypr and
various related tools, since using a Tiling WM often means configuring a lot of your own desktop features
that you generally take for granted.

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
