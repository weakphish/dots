# Zoxide (smart cd)
zoxide init fish | source

# Starship prompt
function starship_transient_prompt_func
  starship module directory && starship module character
end
starship init fish | source
enable_transience

# uv (Python package manager)
uv generate-shell-completion fish | source

# fzf keybindings
fzf --fish | source

# VS Code shell integration
string match -q "$TERM_PROGRAM" vscode
and . (code --locate-shell-integration-path fish)
