{ config, pkgs, ... }:

{
  programs.starship = {
    enable = true;
    enableFishIntegration = true;

    settings = {
      "$schema" = "https://starship.rs/config-schema.json";
      add_newline = false;

      format = ''
        [┌─](bold color_green)$username$hostname$directory$git_branch$git_commit$git_status$python$pulumi
        [└─](bold color_green)$character
      '';

      palette = "everforest";

      palettes.everforest = {
        color_fg0 = "#ebdbb2";
        color_bg1 = "#282828";
        color_bg3 = "#665c54";
        color_blue = "#83a598";
        color_aqua = "#8ec07c";
        color_green = "#b8bb26";
        color_orange = "#fe8019";
        color_purple = "#d3869b";
        color_red = "#fb4934";
        color_yellow = "#fabd2f";
      };

      character = {
        success_symbol = "[󰘧](bold color_green)";
      };

      git_branch = {
        symbol = "";
        style = "color_blue";
        format = " [$symbol $branch]($style)";
        ignore_branches = [ "master" "main" ];
      };

      git_status = {
        style = "color_blue";
        format = " [$all_status$ahead_behind]($style)";
      };

      directory = {
        truncate_to_repo = false;
        format = " in [$path]($style)";
      };

      kubernetes = {
        disabled = false;
        format = " on [$symbol$context(\\($namespace\\))]($style)";
      };

      hostname = {
        ssh_only = false;
        ssh_symbol = "";
        format = " on [$ssh_symbol$hostname](color_green)";
        trim_at = ".";
        disabled = false;
      };

      pulumi = {
        format = " via [$symbol($username@)$stack]($style) ";
      };

      username = {
        style_user = "color_green";
        style_root = "color_fg0";
        format = "[$user]($style)";
        disabled = false;
        show_always = true;
      };
    };
  };
}
