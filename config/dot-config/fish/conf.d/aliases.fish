# Shell aliases
# Sourced automatically by Fish from conf.d/

# Git and development tools
alias lg lazygit
alias ldo lazydocker
alias tf terraform
alias p "poetry run"
alias dc "docker compose"
alias k kubectl
alias pu pulumi
alias oc opencode

# Directory navigation
alias cdr z

# Better CLI tools
alias ls lsd
alias la "lsd -la"
alias cat bat

# Kubernetes
alias kubeseal "kubeseal --controller-name sealed-secrets --controller-namespace=default"

# FZF log viewer
alias fzflog "fzf --tail 100000 --tac --no-sort --exact --wrap"

# OS-specific aliases
switch (uname)
    case Linux
        alias pacs "pacman -S"
end
