# Fish shell configuration
#
# Modular config structure:
#   conf.d/* 			- Config modules
#   conf.d/local.fish   - Machine-specific config (gitignored)
#   functions/          - Autoloaded functions

# Editor
set -gx EDITOR nvim

# Vi keybindings
fish_vi_key_bindings

# Bat theme
set -gx BAT_THEME base16
