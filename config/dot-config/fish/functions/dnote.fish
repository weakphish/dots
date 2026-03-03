function dnote --description "Manage daily notes: open, add entries, or view with glow"
    if set -q _flag_help; or contains -- --help $argv
        echo "Usage: dnote              Open today's note in Zed"
        echo "       dnote add [-i N] [text]"
        echo "                          Append a timestamped bullet (gum prompt if no text)"
        echo "       dnote view         Render today's note with glow"
        echo "       dnote view all     Render all notes with glow"
        echo ""
        echo "Requires DAILY_NOTES_DIR to be set."
        return 0
    end

    if not set -q DAILY_NOTES_DIR
        echo "DAILY_NOTES_DIR is not set" >&2
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

    if test (count $argv) -eq 0
        zed "$file"
        return
    end

    switch $argv[1]
        case add
            set -e argv[1]
            _dnote_add "$file" $argv
        case view
            set -e argv[1]
            _dnote_view "$file" $argv
        case '*'
            echo "Unknown subcommand: $argv[1]" >&2
            echo "Usage: dnote [add [-i N] <text> | view [all]]" >&2
            return 1
    end
end

function _dnote_add --description "Append a timestamped bullet to a daily note file"
    set -l file $argv[1]
    set -e argv[1]

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
        echo "Usage: dnote add [-i N] <text>" >&2
        return 1
    end

    if test -z "$text"
        return 1
    end

    set -l timestamp (date +%H:%M)
    echo "$indent- [$timestamp] $text" >>"$file"
end

function _dnote_view --description "Render daily notes using glow"
    set -l file $argv[1]
    set -e argv[1]

    if not command -q glow
        echo "glow is not installed. Install it with: brew install glow" >&2
        return 1
    end

    if test (count $argv) -eq 0
        glow "$file"
        return
    end

    switch $argv[1]
        case all
            set -l files $DAILY_NOTES_DIR/*/*/*.md
            for file in $files[-1..1]
                cat $file
                echo
            end | glow -p -
        case '*'
            echo "Unknown view argument: $argv[1]" >&2
            echo "Usage: dnote view [all]" >&2
            return 1
    end
end
