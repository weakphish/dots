# Dotfiles, managed with GNU Stow

## What

At work, I use MacOS primarily, but the tools listed below I use both at work and at home.

At home, I use MacOS on my personal laptop, and keep a Windows gaming computer despite my intense dislike for Windows.

### Main Tools
- Code Editor: Neovim and Zed, depending on the task.
- Shell: Fish
- Terminal: Ghostty
- Zellij as a tmux replacement/session-manager/terminal 10x improvement
- Git Porcelain: LazyGit

### Agents, etc.
I mainly use Codex, but am experimenting with OpenCode and Pi. For Pi, my config is stored separately [here](https://github.com/weakphish/pi-config).

My 'primary' `AGENTS.md` lives in the Codex config, which is found in this repo at `config/dot-codex`.

For Agent Skills, I use the `~/.agents/skills` directory (found at `config/dot-agents/skills` in this repo) as it is compatible with both Codex and OpenCode.

For OpenCode, I have my [global](https://opencode.ai/docs/config/#global) config stored in this repo,
and then overlay a custom config for work-sensitive stuff using a [custom directory](https://opencode.ai/docs/config/#custom-directory).

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

## Structure
- `config/` houses my dotfiles, which are symlinked with Stow
- `karabiner/` contains Karabiner-Elements configuration for macOS key remapping

## Usage
Run the bootstrap script for the appropriate operating system, then run `stow config --target ~/ --dotfiles`

The bootstrap script for Mac just installs Homebrew and Brewfile, whereas the Arch one just installs Ansible and configures yay for the AUR.
