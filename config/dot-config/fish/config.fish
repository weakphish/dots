# Fish shell configuration
# Noisy config: environment variables, PATH modifications, and tool initializations
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

#################
### PATH Setup ###
#################

# Pulumi
fish_add_path $HOME/.pulumi/bin

# Local binaries (amp, pipx, etc.)
fish_add_path $HOME/.local/bin

##########################
### Tool Initializations ###
##########################

# Zoxide (smart cd)
zoxide init fish | source

# Starship prompt
starship init fish | source

# uv (Python package manager)
uv generate-shell-completion fish | source

# fzf keybindings
fzf --fish | source

# VS Code shell integration
string match -q "$TERM_PROGRAM" vscode
and . (code --locate-shell-integration-path fish)
