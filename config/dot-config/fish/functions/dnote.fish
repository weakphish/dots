function dnote --description "Manage daily notes: open, add entries, or view with glow"
    if set -q _flag_help; or contains -- --help $argv
        echo "Usage: dnote              Open today's note in \$EDITOR"
        echo "       dnote add [-i N] [text]"
        echo "                          Append a timestamped bullet (gum prompt if no text)"
        echo "       dnote view         Render today's note with glow"
        echo "       dnote view all     Render all notes with glow"
        echo "       dnote ask [question]"
        echo "       dnote ask [-t|--today-only] [question]"
        echo "                          Ask OpenCode in DAILY_NOTES_DIR (or only today's note)"
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
    set -l weekday (date +%A)
    set -l dir "$DAILY_NOTES_DIR/$year/$month"
    set -l file "$dir/$today.md"

    if not test -f "$file"
        mkdir -p "$dir"
        echo "# $today $weekday" >"$file"
    end

    if test (count $argv) -eq 0
        $EDITOR "$file"
        return
    end

    switch $argv[1]
        case add
            set -e argv[1]
            _dnote_add "$file" $argv
        case view
            set -e argv[1]
            _dnote_view "$file" $argv
        case ask
            set -e argv[1]
            _dnote_ask "$file" $argv
        case '*'
            echo "Unknown subcommand: $argv[1]" >&2
            echo "Usage: dnote [add [-i N] <text> | view [all] | ask [question]]" >&2
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

# Ask OpenCode a question in the notes workspace.
#
# Why this exists:
# - Runs OpenCode with DAILY_NOTES_DIR as the working context so AGENTS.md in
#   that folder can guide behavior for note-focused Q&A.
#
# Parameters:
# - $argv[1]: Path to today's note file (string).
# - $argv[2..]: Flags and question tokens (optional); if omitted and gum is
#   installed, prompts interactively.
#
# Returns:
# - 0 on successful OpenCode invocation.
# - 1 when prerequisites fail or no question is provided.
function _dnote_ask --description "Ask OpenCode about notes in DAILY_NOTES_DIR"
    set -l file $argv[1]
    set -e argv[1]

    if not command -q opencode
        echo "opencode is not installed or not in PATH" >&2
        return 1
    end

    argparse 't/today-only' -- $argv
    or return 1

    set -l question
    if test (count $argv) -gt 0
        set question "$argv"
    else if command -q gum
        set question (gum input --placeholder "Ask about your notes, boss" --prompt="? " --width 120)
        or return 1
    else
        echo "Usage: dnote ask [-t|--today-only] [question]" >&2
        return 1
    end

    if test -z "$question"
        return 1
    end

    if not test -f "$DAILY_NOTES_DIR/AGENTS.md"
        echo "Warning: $DAILY_NOTES_DIR/AGENTS.md not found; continuing without folder-specific agent instructions." >&2
    end

    if set -q _flag_today_only
        command opencode run --dir "$DAILY_NOTES_DIR" -f "$file" -- "$question"
    else
        command opencode run --dir "$DAILY_NOTES_DIR" -- "$question"
    end
end
