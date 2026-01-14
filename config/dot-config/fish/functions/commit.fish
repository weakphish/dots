# Interactive git commit helper using gum for styled prompts.
# Guides user through conventional commit format selection.
#
# Usage:
#   commit            - Start interactive commit flow
#   commit -h|--help  - Show this help message
#
# Workflow:
#   1. Select commit type (fix, feat, docs, style, refactor, test, chore, revert)
#   2. Enter optional scope
#   3. Write commit summary
#   4. Write optional detailed description
#   5. Confirm and commit
#
# Requirements:
#   gum - https://github.com/charmbracelet/gum
function commit --description "Interactive git commit helper using gum"
    argparse h/help -- $argv
    or return 1

    if set -q _flag_help
        echo "Usage: commit [-h|--help]"
        echo ""
        echo "Interactive git commit helper using gum for styled prompts."
        echo "Guides user through conventional commit format selection."
        echo ""
        echo "Options:"
        echo "  -h, --help    Show this help message"
        echo ""
        echo "Workflow:"
        echo "  1. Select commit type (fix, feat, docs, style, refactor, test, chore, revert)"
        echo "  2. Enter optional scope"
        echo "  3. Write commit summary"
        echo "  4. Write optional detailed description"
        echo "  5. Confirm and commit"
        echo ""
        echo "Requirements:"
        echo "  gum - https://github.com/charmbracelet/gum"
        return 0
    end

    set TYPE (gum choose "fix" "feat" "docs" "style" "refactor" "test" "chore" "revert")
    set SCOPE (gum input --placeholder "scope")
    set SUMMARY (gum input --value "$TYPE-$SCOPE: " --placeholder "Summary of this change")
    set DESCRIPTION (gum write --placeholder "Details of this change")
    gum confirm "Commit changes?" && git commit -m "$SUMMARY" -m "$DESCRIPTION"
end
