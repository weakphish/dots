# Fish shell configuration
#
# Modular config structure:
#   conf.d/* 			- Config modules
#   conf.d/local.fish   - Machine-specific config (gitignored)
#   functions/          - Autoloaded functions

# Vi keybindings
fish_vi_key_bindings

# Autostart Zellij
if status is-interactive
    eval (zellij setup --generate-auto-start fish | string collect)
end

