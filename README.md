# Dotfiles, managed with GNU Stow

# What

At work, I use MacOS primarily, but the tools listed below I use both at work and at home.

**Code Editor**: Neovim, with JetBrains tooling for large refactors/debugging/anything Neovim struggles with.

I've also been experimenting with using Zed, which has caught my attention
as a performant alternative to VS Code that also has cleaner Vim integration.

**Shell:** Fish

**Terminal:** Ghostty on macOS/Linux, Windows Terminal on Windows.

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

Currently, I use Hyprland as a tiling window manager. Included in this repo is configuration for Hypr and
various related tools, since using a Tiling WM often means configuring a lot of your own desktop features
that you generally take for granted.

Also included are Kvantum and GTK themes, as I like Everforest a lot.

## Game Development
For game development projects, I tend to stick to Godot. I use VS Code to edit GDScript, just because the built-in editor
is pretty basic, and I couldn't get Neovim to work with Godot reasonably well.

For C#, I use JetBrains Rider.

# Usage

Run `stow config --dotfiles` with this repo cloned to `$HOME/dots`
