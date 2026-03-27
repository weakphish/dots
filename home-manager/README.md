# Home Manager Configuration

This directory contains the Nix Home Manager configuration for managing dotfiles declaratively.

## Structure

```
home-manager/
├── home.nix           # Linux (Arch/NixOS) configuration
├── home-darwin.nix    # macOS configuration
└── modules/           # Individual program modules
    ├── fish.nix       # Fish shell
    ├── starship.nix   # Starship prompt
    ├── git.nix        # Git with delta
    ├── neovim.nix     # Neovim with LSPs
    ├── helix.nix      # Helix editor
    ├── hyprland.nix   # Hyprland window manager (Linux only)
    ├── waybar.nix     # Waybar status bar (Linux only)
    ├── dunst.nix      # Dunst notifications (Linux only)
    └── ghostty.nix    # Ghostty terminal
```

## Usage

### Prerequisites

1. Install Nix with flakes enabled:
   ```bash
   sh <(curl -L https://nixos.org/nix/install) --daemon
   ```

2. Enable flakes in your Nix configuration:
   ```bash
   mkdir -p ~/.config/nix
   echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
   ```

### Building and Activating

From the repository root:

**Linux:**
```bash
nix run home-manager/master -- switch --flake .#weakphish
```

**macOS:**
```bash
nix run home-manager/master -- switch --flake .#weakphish@macos
```

### Updating

To update all flake inputs:
```bash
nix flake update
```

To rebuild after making changes:
```bash
home-manager switch --flake .#weakphish
```

## Customization

1. Edit `home.nix` or `home-darwin.nix` to add/remove packages
2. Modify modules in the `modules/` directory for program-specific settings
3. The original config files in `config/dot-config/` are symlinked for complex configurations (neovim, emacs, zed)

## Notes

- Username is set to `weakphish` - update in `home.nix` and `home-darwin.nix` if different
- Git user info is commented out in `modules/git.nix` - uncomment and configure
- Hyprland settings include NVIDIA environment variables - remove if not using NVIDIA GPU
