# Dotfiles, managed with Chezmoi

## What

At work, I use MacOS primarily, but the tools listed below I use both at work and at home.

At home, I use MacOS on my personal laptop, and keep a gaming computer running CachyOS.

### Main Tools

- Code Editor: Neovim and Zed, depending on the task.
- Shell: Fish
- Terminal: Ghostty
- Zellij as a tmux replacement/session-manager/terminal 10x improvement
- Git Porcelain: LazyGit

### Agents, etc.

I mainly use Pi, which has a config at `./dot_pi/`

My 'primary' `AGENTS.md` lives in the Pi config.

For Agent Skills, I use the `~/.agents/skills` directory (found at `dot_agents/skills` in this repo) as it is compatible with most harnesses.

### Other Software

Other software that I use as a developer on a regular basis:

- [A few Rust utilities](https://gist.github.com/sts10/daadbc2f403bdffad1b6d33aff016c0a)
    - Bat
    - Delta
    - Lsd
    - Ripgrep
- Good ol' pen and paper for note-taking

### Game Development

For game development projects, I tend to stick to Godot. I use JetBrains Rider to edit GDScript/C#, just because the built-in editor
is pretty basic and doesn't support C# great, and I couldn't get Neovim to work with Godot reasonably well.

## Usage

The bootstrap script for Mac just installs Homebrew and Brewfile, whereas the Arch one just installs Ansible and configures yay for the AUR.
The application of my config files itself is done w/ Chezmoi.
