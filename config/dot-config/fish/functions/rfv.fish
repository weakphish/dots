# Interactive ripgrep->fzf->nvim integration.
# Searches files with ripgrep, displays results in fzf with bat preview,
# and opens selected matches in Neovim.
#
# Based on: https://junegunn.github.io/fzf/tips/ripgrep-integration/#wrap-up
#
# Usage:
#   rfv [QUERY]
#   rfv -h|--help
#
# Keybindings:
#   Enter   - Open current line in Neovim (or build quickfix for selections)
#   Ctrl-o  - Execute opener without closing fzf
#   Alt-a   - Select all
#   Alt-d   - Deselect all
#   Ctrl-/  - Toggle preview
function rfv --description "ripgrep->fzf->nvim integration"
    argparse h/help -- $argv
    or return 1

    if set -q _flag_help
        echo "Usage: rfv [QUERY] [-h|--help]"
        echo ""
        echo "Interactive ripgrep->fzf->nvim integration."
        echo "Searches files with ripgrep, displays results in fzf with bat preview,"
        echo "and opens selected matches in Neovim."
        echo ""
        echo "Options:"
        echo "  -h, --help    Show this help message"
        echo ""
        echo "Arguments:"
        echo "  QUERY         Optional initial search query"
        echo ""
        echo "Keybindings:"
        echo "  Enter         Open current line in Neovim (or build quickfix for selections)"
        echo "  Ctrl-o        Execute opener without closing fzf"
        echo "  Alt-a         Select all"
        echo "  Alt-d         Deselect all"
        echo "  Ctrl-/        Toggle preview"
        echo ""
        echo "Requirements:"
        echo "  rg (ripgrep), fzf, bat, nvim"
        return 0
    end

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
