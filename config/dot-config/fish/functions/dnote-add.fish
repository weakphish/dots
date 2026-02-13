function dnote-add --description "Append a bullet point to today's daily note"
    if not set -q DAILY_NOTES_DIR
        echo "DAILY_NOTES_DIR is not set" >&2
        return 1
    end

    argparse 'i/indent=!_validate_int --min 1' -- $argv
    or return 1

    set -l indent ""
    if set -q _flag_indent
        set indent (string repeat -n (math "$_flag_indent * 4") " ")
    end

    set -l text
    if test (count $argv) -gt 0
        set text "$argv"
    else if command -q gum
        set text (gum input --placeholder "What'd you do, boss?" --prompt="$indent- " --width 80)
        or return 1
    else
        echo "Usage: dnote-add [-i N] <text>" >&2
        return 1
    end

    if test -z "$text"
        return 1
    end

    set -l year (date +%Y)
    set -l month (date +%m)
    set -l today (date +%Y-%m-%d)
    set -l dir "$DAILY_NOTES_DIR/$year/$month"
    set -l file "$dir/$today.md"

    if not test -f "$file"
        mkdir -p "$dir"
        echo "# $today" >"$file"
    end

    echo "$indent- $text" >>"$file"
end
