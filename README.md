# Dotfiles, managed with mise

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
- `config/` houses my dotfiles, symlinked into `$HOME` by mise's `[dotfiles]` feature.
- `karabiner/` contains Karabiner-Elements configuration for macOS key remapping.
- `mise.toml` — base config: cross-platform CLI tools (`[tools]`) and shared dotfiles (`[dotfiles]`).
- `mise.mac.toml` / `mise.linux.toml` / `mise.work.toml` — per-profile overlays (system packages,
  platform-only dotfiles), layered via `MISE_ENV`.

Everything (CLI tools, system packages (Homebrew/pacman), and dotfile symlinks) is driven by
`mise`. There is no Brewfile or bootstrap script; `mise bootstrap` does it all.

## Profiles

The machine picks its profile through `MISE_ENV`.

| Machine        | `MISE_ENV`  | Loads                               |
|----------------|-------------|-------------------------------------|
| Personal mac   | `mac`       | `mise.toml` + `mise.mac.toml`       |
| Linux          | `linux`     | `mise.toml` + `mise.linux.toml`     |
| Work (macOS)   | `mac,work`  | `mise.toml` + `mise.mac.toml` + `mise.work.toml` |

`work` is an overlay *on top of* mac, not a separate OS profile. Profiles applied later take precedence.

## Usage

### Fresh machine

```sh
# 1. Install the two prerequisites mise can't install itself.
#    macOS: install Homebrew (https://brew.sh), then `brew install mise`.
#    Arch:  `sudo pacman -S mise` (or the AUR build).

# 2. Tell mise which profile this machine is.
echo 'set -Ux MISE_ENV mac' # or: linux | mac,work

# 3. From the repo root, ALWAYS preview before touching the machine.
cd ~/Developer/dots
MISE_ENV=mac mise bootstrap --dry-run        # match the profile you set above

# 4. If the plan looks right, run it for real.
MISE_ENV=mac mise bootstrap                  # packages -> dotfiles -> tools, idempotent
```

`mise bootstrap` runs, in order: `[bootstrap.packages]` (Homebrew/pacman, incl. the
`pre-packages` hook for casks mise can't install natively), then `mise dotfiles apply`,
then `mise install` for the `[tools]`. Add `--force-dotfiles` to overwrite conflicting
files. Then reload your shell (`exec fish`) so mise activates and `MISE_ENV` takes effect.

To re-link dotfiles only (no package work): `mise dotfiles apply --dry-run` then
`mise dotfiles apply`. `mise dotfiles status` shows what's linked vs. drifted. Everything is
idempotent.
