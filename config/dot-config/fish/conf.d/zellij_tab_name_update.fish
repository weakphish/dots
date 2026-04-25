status is-interactive; or return

# Keep the Zellij tab title in sync with the current directory.
function zellij_tab_name_update --on-variable PWD
    if not set -q ZELLIJ
        return
    end

    if not command -sq zellij
        return
    end

    set -l path_parts (string split / -- "$PWD")
    set -l dir_name $path_parts[-1]

    if test -z "$dir_name"
        set dir_name "/"
    end

    set -l tab_name "$dir_name"

    if command -sq git
        set -l repo_root (command git rev-parse --show-toplevel 2>/dev/null)
        if test -n "$repo_root"
            set -l repo_parts (string split / -- "$repo_root")
            set -l repo_name $repo_parts[-1]

            if test -n "$repo_name"; and test "$repo_name" != "$dir_name"
                set tab_name "$repo_name:$dir_name"
            else if test -n "$repo_name"
                set tab_name "$repo_name"
            end
        end
    end

    set -l max_tab_name_length 28
    set -l tab_name_length (string length -- "$tab_name")
    if test "$tab_name_length" -gt "$max_tab_name_length"
        set -l suffix_length (math "$max_tab_name_length - 3")
        set tab_name "..."(string sub -s -$suffix_length -- "$tab_name")
    end

    if set -q __zellij_last_tab_name; and test "$__zellij_last_tab_name" = "$tab_name"
        return
    end

    if command zellij action rename-tab -- "$tab_name" >/dev/null 2>&1
        set -g __zellij_last_tab_name "$tab_name"
        return
    end

    printf "zellij_tab_name_update: failed to rename tab to '%s'\n" "$tab_name" >&2
end

# Rename once on startup for existing tabs.
if set -q ZELLIJ
    zellij_tab_name_update
end
