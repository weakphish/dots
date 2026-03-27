{ config, pkgs, inputs, ... }:

{
  imports = [
    ./modules/fish.nix
    ./modules/starship.nix
    ./modules/git.nix
    ./modules/neovim.nix
    ./modules/helix.nix
    ./modules/hyprland.nix
    ./modules/waybar.nix
    ./modules/dunst.nix
    ./modules/ghostty.nix
  ];

  home = {
    username = "weakphish";
    homeDirectory = "/home/weakphish";
    stateVersion = "24.11";

    packages = with pkgs; [
      # CLI utilities
      bat
      delta
      lsd
      ripgrep
      fd
      fzf
      zoxide
      lazygit
      lazydocker
      gum
      jq
      yq

      # Development tools
      uv
      nodejs
      go
      rustup

      # Cloud/DevOps
      kubectl
      k9s
      pulumi
      terraform
      docker-compose

      # Hyprland ecosystem
      hyprpaper
      hyprlock
      hypridle
      hyprlauncher
      hyprsunset
      grim
      slurp
      wl-clipboard
      brightnessctl
      playerctl
      pamixer

      # Desktop
      firefox
      thunar

      # Fonts
      (nerdfonts.override { fonts = [ "JetBrainsMono" "FiraCode" ]; })
    ];

    sessionVariables = {
      EDITOR = "nvim";
      BAT_THEME = "base16";
    };

    file = {
      # Neovim full config (too complex for home-manager module)
      ".config/nvim" = {
        source = ../config/dot-config/nvim;
        recursive = true;
      };

      # Emacs config
      ".config/emacs" = {
        source = ../config/dot-config/emacs;
        recursive = true;
      };

      # Doom Emacs config
      ".config/doom" = {
        source = ../config/dot-config/doom;
        recursive = true;
      };

      # Zed editor
      ".config/zed" = {
        source = ../config/dot-config/zed;
        recursive = true;
      };

      # k9s
      ".config/k9s" = {
        source = ../config/dot-config/k9s;
        recursive = true;
      };

      # Hyprland additional configs
      ".config/hypr/hyprpaper.conf".source = ../config/dot-config/hypr/hyprpaper.conf;
      ".config/hypr/hyprlock.conf".source = ../config/dot-config/hypr/hyprlock.conf;
      ".config/hypr/hypridle.conf".source = ../config/dot-config/hypr/hypridle.conf;
      ".config/hypr/hyprsunset.conf".source = ../config/dot-config/hypr/hyprsunset.conf;
      ".config/hypr/hyprtoolkit.conf".source = ../config/dot-config/hypr/hyprtoolkit.conf;

      # Wallpaper
      "Pictures/wallpaper.png".source = ../wallpaper.png;
    };
  };

  # Let Home Manager manage itself
  programs.home-manager.enable = true;

  # Enable font configuration
  fonts.fontconfig.enable = true;
}
