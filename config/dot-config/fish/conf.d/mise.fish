# Activate mise so its [tools] (starship, zoxide, uv, fzf, ...) land on PATH.
#
# This file sorts before tools.fish, so the tools it provides are available
# when tools.fish runs `starship init`, `zoxide init`, etc.
#
# MISE_ENV (which dotfiles/tools profile this machine uses) 
#   mac          ->  set -gx MISE_ENV mac
#   linux        ->  set -gx MISE_ENV linux
#   work (macOS) ->  set -gx MISE_ENV mac,work

if type -q mise
    mise activate fish | source
end
