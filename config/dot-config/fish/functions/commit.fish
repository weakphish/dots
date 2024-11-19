function commit
    set TYPE (gum choose "fix" "feat" "docs" "style" "refactor" "test" "chore" "revert")
    set SCOPE (gum input --placeholder "scope")
    set SUMMARY (gum input --value "$TYPE-$SCOPE: " --placeholder "Summary of this change")
    set DESCRIPTION (gum write --placeholder "Details of this change")
    gum confirm "Commit changes?" && git commit -m "$SUMMARY" -m "$DESCRIPTION"
end
