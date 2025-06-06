set -gx EDITOR nvim

fish_vi_key_bindings

# Aliases
alias lg lazygit
alias ldo lazydocker

alias pu pulumi

alias tf terraform
alias p "poetry run"
alias dc "docker compose"
alias k kubectl

alias ls lsd
alias la "lsd -la"
alias cat bat

# OS-specific Aliases
switch (uname)
    case Linux
        alias pacs "pacman -S"
end

# VS Code integration
string match -q "$TERM_PROGRAM" vscode
and . (code --locate-shell-integration-path fish)

# Bat Theme
set -Ux BAT_THEME base16

zoxide init fish | source

starship init fish | source

uv generate-shell-completion fish | source

# Created by `pipx` on 2025-01-24 23:34:29
set PATH $PATH /Users/weakphish/.local/bin

# Set up fzf key bindings
fzf --fish | source
