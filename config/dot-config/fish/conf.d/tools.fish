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
