{ config, pkgs, ... }:

{
  # Ghostty config - using xdg.configFile since there's no native module yet
  xdg.configFile."ghostty/config".text = ''
    theme = Gruvbox Dark

    keybind = global:cmd+option+h=goto_split:left
    keybind = global:cmd+option+l=goto_split:right
    keybind = global:cmd+option+j=goto_split:down
    keybind = global:cmd+option+k=goto_split:up

    keybind = global:cmd+shift+space=toggle_quick_terminal
    quick-terminal-animation-duration = 0.1

    window-decoration=none
  '';

  home.packages = with pkgs; [
    ghostty
  ];
}
