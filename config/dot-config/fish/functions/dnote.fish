function dnote --description "Create and open today's daily note in Zed"
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

    zed "$file"
end
