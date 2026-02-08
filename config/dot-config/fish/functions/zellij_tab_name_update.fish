# Found at https://www.reddit.com/r/zellij/comments/1dn4e7i/comment/la4kbqz/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
function zellij_tab_name_update --on-variable PWD
    if set -q ZELLIJ
        set dirname $PWD
        set dirname (string split / $dirname)
        set dirname $dirname[-1]
        command nohup zellij action rename-tab $dirname >/dev/null 2>&1
    end
end
