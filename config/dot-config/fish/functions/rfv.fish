# Interactive ripgrep->fzf->nvim integration.
# Searches files with ripgrep, displays results in fzf with bat preview,
# and opens selected matches in Neovim.
#
# Based on: https://junegunn.github.io/fzf/tips/ripgrep-integration/#wrap-up
#
# Usage:
#   rfv [QUERY]
#
# Keybindings:
#   Enter   - Open current line in Neovim (or build quickfix for selections)
#   Ctrl-o  - Execute opener without closing fzf
#   Alt-a   - Select all
#   Alt-d   - Deselect all
#   Ctrl-/  - Toggle preview
function rfv --description "ripgrep->fzf->nvim integration"
    set -l RELOAD 'reload:rg --column --color=always --smart-case {q} || :'
    set -l OPENER 'if [[ $FZF_SELECT_COUNT -eq 0 ]]; then
            nvim {1} +{2}     # No selection. Open the current line in Nvim.
          else
            nvim +cw -q {+f}  # Build quickfix list for the selected items.
          fi'

    fzf --disabled --ansi --multi \
        --bind "start:$RELOAD" --bind "change:$RELOAD" \
        --bind "enter:become:$OPENER" \
        --bind "ctrl-o:execute:$OPENER" \
        --bind 'alt-a:select-all,alt-d:deselect-all,ctrl-/:toggle-preview' \
        --delimiter : \
        --preview 'bat --style=full --color=always --highlight-line {2} {1}' \
        --preview-window '~4,+{2}+4/3,<80(up)' \
        --query "$argv"
end
