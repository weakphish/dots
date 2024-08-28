set -gx EDITOR nvim

# use vim keybinds if not inside neovim terminal
if set -q $NVIM
    fish_default_key_bindings
else
    fish_vi_key_bindings
end

# Aliases
alias lg lazygit
alias tf terraform
alias p "poetry run"
alias dc "docker compose"
alias k kubectl
alias cat bat
alias ls lsd
alias la "lsd -la"
alias ch chezmoi

# iTerm integration
test -e {$HOME}/.iterm2_shell_integration.fish; and source {$HOME}/.iterm2_shell_integration.fish

# VS Code integration
string match -q "$TERM_PROGRAM" vscode
and . (code --locate-shell-integration-path fish)

# Bat Theme
set -Ux BAT_THEME base16

# Set up Zoxide
zoxide init fish | source

# Setup Starship
starship init fish | source
